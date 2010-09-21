{-# LANGUAGE ForeignFunctionInterface #-}

module NXT.NXT where

import qualified Data.ByteString as B
import Control.Exception
import Control.Monad.State
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import Data.Ratio
import Data.Time.Clock.POSIX
import Data.Word
import Foreign.C.Error
import Foreign.C.Types
import System.IO
import System.Posix.IO
import System.Posix.Signals
import System.Posix.Types
import Text.Printf

import NXT.Data
import NXT.Errors
import NXT.Types

-- Described in Lego Mindstorms NXT Bluetooth Developer Kit:
--  Appendix 1 - Communication protocol
--  Appendix 2 - Direct commands

-- TODO: All functions which requests ModuleInfo could populate module ID cache along the way

-- Foreign function call for C function which initialize serial port device on POSIX systems
foreign import ccall unsafe "initserial.h" initSerialPort :: Fd -> IO CInt

-- TODO: Move to configuration file
device :: FilePath -- serial port device file
--device = "/dev/tty.NatriX-DevB-1"
device = "/dev/rfcomm0"

-- TODO: Move to configuration file
debug :: Bool
debug = False

-- Opens and intializes serial port, installs signal handler so that ctrl-c closes the program gracefully
initialize :: IO NXTState
initialize = do
  -- we have to block signals from interrupting openFd system call (fixed in GHC versions after 6.12.1)
  let signals = foldl (flip addSignal) emptySignalSet [virtualTimerExpired]
  blockSignals signals
  fd <- openFd device ReadWrite Nothing OpenFileFlags { append = False, noctty = True, exclusive = False, nonBlock = True, trunc = False }
  unblockSignals signals
  throwErrnoIfMinus1_ "initSerialPort" $ initSerialPort fd
  h <- fdToHandle fd
  hSetBuffering h NoBuffering
  when debug $ hPutStrLn stderr "initialized"
  return $ NXTState h Nothing [] 0 0

-- Stops all NXT activities and closes the handler (and so serial port connection)
terminate :: NXTState -> IO ()
terminate nxtstate = do
  nxtstate' <- execStateT stopEverything nxtstate
  let h = nxthandle nxtstate'
  hClose h
  when debug $ hPutStrLn stderr "terminated"

-- Main function for sending data to NXT
-- It calculates the length and prepends it to the message
sendData :: [Word8] -> NXT ()
sendData message = do
  h <- gets nxthandle
  let len = toUWord . length $ message
      packet = len ++ message
  liftIO . B.hPut h . B.pack $ packet
  when debug $ liftIO . hPutStrLn stderr $ "sent: " ++ show packet

-- Main function for receiving data from NXT
receiveData :: NXT [Word8]
receiveData = do
  h <- gets nxthandle
  len <- liftIO $ B.hGet h 2
  let len' = fromUWord . B.unpack $ len
  packet <- liftIO $ B.hGet h len'
  let unpacket = B.unpack packet
  when debug $ liftIO . hPutStrLn stderr $ "received: " ++ show unpacket
  return unpacket

-- Gets firmware and protocol versions
getVersion :: NXT Version
getVersion = do
  when debug $ liftIO . hPutStrLn stderr $ "getversion"
  let send = [0x01, 0x88]
  sendData send
  receive <- receiveData
  case receive of
    [0x02, 0x88, 0x00, pMinor, pMajor, fMinor, fMajor] ->
      return $ Version (FirmwareVersion fMajor' fMinor') (ProtocolVersion pMajor' pMinor')
        where fMajor' = fromIntegral fMajor
              fMinor' = fromIntegral fMinor
              pMajor' = fromIntegral pMajor
              pMinor' = fromIntegral pMinor
    _:_:e:_                                            -> failNXT "getVersion" e
    _                                                  -> fail "getVersion"

-- Gets device information: name, Bluetooth 48 bit address in the string format, strength of Bluetooth signal, free space on flash
getDeviceInfo :: NXT DeviceInfo
getDeviceInfo = do
  when debug $ liftIO . hPutStrLn stderr $ "getdeviceinfo"
  let send = [0x01, 0x9B]
  sendData send
  receive <- receiveData
  case receive of
    0x02:0x9B:0x00:info | length info == 30 -> do
      modify (\s -> s { address = Just btaddress }) -- we cache it
      return $ DeviceInfo name' btaddress btstrength flashfree
        where (name, info') = splitAt 15 info
              name' = dataToString0 name
              btaddress = map toUpper . concat . intersperse ":" . map (printf "%02x") . take 6 $ info'
              -- 7th byte not used?
              btstrength = fromULong . take 4 . drop 7 $ info'
              flashfree = fromULong . take 4 . drop 11 $ info'
    _:_:e:_               -> failNXT "getDeviceInfo" e
    _                     -> fail "getDeviceInfo"

-- Starts a program
startProgram :: FileName -> NXT ()
startProgram = startProgram' False

-- Starts a program, but also gets the confirmation
startProgramConfirm :: FileName -> NXT ()
startProgramConfirm = startProgram' True

startProgram' :: Bool -> FileName -> NXT ()
startProgram' confirm filename = do
  when debug $ liftIO . hPutStrLn stderr $ "startprogram"
  let send = [request, 0x00] ++ (nameToData filename)
  sendData send
  when confirm $ do
    receive <- receiveData
    case receive of
      [0x02, 0x00, 0x00] -> return ()
      [_, _, e]          -> failNXT "startProgram" e
      _                  -> fail "startProgram"
    where request = if confirm
                      then 0x00
                      else 0x80

-- Stops a program
stopProgram :: NXT ()
stopProgram = stopProgram' False False

-- Stops a program, but also gets the confirmation
stopProgramConfirm :: NXT ()
stopProgramConfirm = stopProgram' True False

-- Deletes a file, but requires program running
stopProgramExisting :: FileName -> NXT ()
stopProgramExisting = delete' True True

stopProgram' :: Bool -> Bool -> NXT ()
stopProgram' confirm running = do
  when debug $ liftIO . hPutStrLn stderr $ "stopprogram"
  let send = [request, 0x01]
  sendData send
  when confirm $ do
    receive <- receiveData
    case receive of
      [0x02, 0x01, 0x00] -> return ()
      [0x02, 0x01, 0xEC] -> if running
                              then failNXT "stopProgram" 0xEC
                              else return ()
      [_, _, e]          -> failNXT "stopProgram" e
      _                  -> fail "stopProgram"
    where request = if confirm
                      then 0x00
                      else 0x80

-- Plays a sound file
playSoundFile :: Bool -> FileName -> NXT ()
playSoundFile = playSoundFile' False

-- Plays a sound file, but also gets the confirmation
playSoundFileConfirm :: Bool -> FileName -> NXT ()
playSoundFileConfirm = playSoundFile' True

playSoundFile' :: Bool -> Bool -> FileName -> NXT ()
playSoundFile' confirm loop filename = do
  when debug $ liftIO . hPutStrLn stderr $ "playsoundfile"
  let send = [request, 0x02, fromIntegral . fromEnum $ loop] ++ (nameToData filename)
  sendData send
  when confirm $ do
    receive <- receiveData
    case receive of
      [0x02, 0x02, 0x00] -> return ()
      [_, _, e]          -> failNXT "playSoundFile" e
      _                  -> fail "playSoundFile"
    where request = if confirm
                      then 0x00
                      else 0x80

-- Plays a tone with given frequency (in Hz) for given duration (in s)
playTone :: Frequency -> Duration -> NXT ()
playTone frequency duration = do
  when debug $ liftIO . hPutStrLn stderr $ "playtone"
  let send = [0x80, 0x03] ++ (toUWord frequency) ++ (toUWord $ toMilliseconds duration)
  sendData send
    where toMilliseconds :: Duration -> Integer -- duration is in seconds, but NXT requires milliseconds
          toMilliseconds d = floor (d * 1000)

-- Sets output (motor) state
setOutputState :: OutputPort -> OutputPower -> [OutputMode] -> RegulationMode -> TurnRatio -> RunState -> TachoLimit -> NXT ()
setOutputState = setOutputState' False

-- Sets output (motor) state, but also gets the confirmation
setOutputStateConfirm :: OutputPort -> OutputPower -> [OutputMode] -> RegulationMode -> TurnRatio -> RunState -> TachoLimit -> NXT ()
setOutputStateConfirm = setOutputState' True

setOutputState' :: Bool -> OutputPort -> OutputPower -> [OutputMode] -> RegulationMode -> TurnRatio -> RunState -> TachoLimit -> NXT ()
setOutputState' confirm output power mode regulation turn runstate tacholimit
  | power >= -100 && power <= 100 && turn >= -100 && turn <= 100 = do
      when debug $ liftIO . hPutStrLn stderr $ "setoutputstate"
      let send = [request, 0x04, fromIntegral . fromEnum $ output] ++ (toSByte power) ++ [modebyte, regulation'] ++ (toSByte turn) ++ [runstate'] ++ (toULong tacholimit)
      sendData send
      when confirm $ do
        receive <- receiveData
        case receive of
          [0x02, 0x04, 0x00] -> return ()
          [_, _, e]          -> failNXT "setOutputState" e
          _                  -> fail "setOutputState"
  | otherwise                                                    = throw $ PatternMatchFail "setOutputState"
    where request     = if confirm
                          then 0x00
                          else 0x80
          modebyte    = foldl (.|.) 0x00 . map convmode $ mode
            where convmode m = case m of
                                 MotorOn -> 0x01
                                 Brake -> 0x02
                                 Regulated -> 0x04
          regulation' = case regulation of
                          RegulationModeIdle -> 0x00
                          RegulationModeMotorSpeed -> 0x01
                          RegulationModeMotorSync -> 0x02
          runstate'   = case runstate of
                          MotorRunStateIdle -> 0x00
                          MotorRunStateRampUp -> 0x10
                          MotorRunStateRunning -> 0x20
                          MotorRunStateRampDown -> 0x40
                          MotorRunStateHold -> 0x60

-- Gets output (motor) state (with rotation)
getOutputState :: OutputPort -> NXT OutputState
getOutputState output = do
  when debug $ liftIO . hPutStrLn stderr $ "getoutputstate"
  let send = [0x00, 0x06, fromIntegral . fromEnum $ output]
  sendData send
  receive <- receiveData
  case receive of
    0x02:0x06:0x00:port:power:modebyte:regulation:turn:runstate:values
      | length values == 16 && fromEnum output == fromIntegral port ->
          return $ OutputState output (fromSByte [power]) mode regulation' (fromSByte [turn]) runstate' tacholimit tachocount blocktachocount rotationcount
            where mode = motoron ++ brake ++ regulated
                  motoron = if testBit modebyte 0
                              then [MotorOn]
                              else []
                  brake = if testBit modebyte 1
                            then [Brake]
                            else []
                  regulated = if testBit modebyte 2
                                then [Regulated]
                                else []
                  regulation' = case regulation of
                                  0x00 -> RegulationModeIdle
                                  0x01 -> RegulationModeMotorSpeed
                                  0x02 -> RegulationModeMotorSync
                                  _    -> throw $ PatternMatchFail "getOutputState"
                  runstate' = case runstate of
                                0x00 -> MotorRunStateIdle
                                0x10 -> MotorRunStateRampUp
                                0x20 -> MotorRunStateRunning
                                0x40 -> MotorRunStateRampDown
                                0x60 -> MotorRunStateHold
                                _    -> throw $ PatternMatchFail "getOutputState"
                  tacholimit = fromULong . take 4 $ values
                  tachocount = fromSLong . take 4 . drop 4 $ values
                  blocktachocount = fromSLong . take 4 . drop 8 $ values
                  rotationcount = fromSLong . take 4 . drop 12 $ values
    _:_:e:_                                                         -> failNXT "getOutputState" e
    _                                                               -> fail "getOutputState"

-- Sets input (sensor) mode
setInputMode :: InputPort -> SensorType -> SensorMode -> NXT ()
setInputMode = setInputMode' False

-- Sets input (sensor) mode, but also gets the confirmation
setInputModeConfirm :: InputPort -> SensorType -> SensorMode -> NXT ()
setInputModeConfirm = setInputMode' True

setInputMode' :: Bool -> InputPort -> SensorType -> SensorMode -> NXT ()
setInputMode' confirm input sensortype sensormode = do
  when debug $ liftIO . hPutStrLn stderr $ "setinputmode"
  let send = [request, 0x05, fromIntegral . fromEnum $ input, sensortype', sensormode']
  sendData send
  when confirm $ do
    receive <- receiveData
    case receive of
      [0x02, 0x05, 0x00] -> return ()
      [_, _, e]          -> failNXT "setInputMode" e
      _                  -> fail "setInputMode"
    where request     = if confirm
                          then 0x00
                          else 0x80
          sensortype' = case sensortype of
                          NoSensor -> 0x00
                          Switch -> 0x01
                          Temperature -> 0x02
                          Reflection -> 0x03
                          Angle -> 0x04
                          LightActive -> 0x05
                          LightInactive -> 0x06
                          SoundDB -> 0x07
                          SoundDBA -> 0x08
                          Custom -> 0x09
                          Lowspeed -> 0x0A
                          Lowspeed9V -> 0x0B
                          NoOfSensorTypes -> 0x0C
          sensormode' = case sensormode of
                          RawMode -> 0x00
                          BooleanMode -> 0x20
                          TransitionCntMode -> 0x40
                          PeriodCounterMode -> 0x60
                          PctFullScaleMode -> 0x80
                          CelsiusMode -> 0xA0
                          FahrenheitMode -> 0xC0
                          AngleStepsMode -> 0xE0
                          -- two modes unnecessary?
                          -- SlopeMask -> 0x1F
                          -- ModeMask -> 0xE0

-- Gets input (sensor) values
getInputValues :: InputPort -> NXT InputValues
getInputValues input = do
  when debug $ liftIO . hPutStrLn stderr $ "getinputvalues"
  let send = [0x00, 0x07, fromIntegral . fromEnum $ input]
  sendData send
  receive <- receiveData
  case receive of
    0x02:0x07:0x00:port:valid:calibrated:sensortype:sensormode:values
      | length values == 8 && fromEnum input == fromIntegral port ->
          return $ InputValues input valid' calibrated' sensortype' sensormode' raw normalized scaled calibratedv
            where valid'      = case valid of
                                  0x00 -> NotValid
                                  _    -> Valid
                  calibrated' = case calibrated of
                                  0x00 -> NotCalibrated
                                  _    -> Calibrated
                  sensortype' = case sensortype of
                                  0x00 -> NoSensor
                                  0x01 -> Switch
                                  0x02 -> Temperature
                                  0x03 -> Reflection
                                  0x04 -> Angle
                                  0x05 -> LightActive
                                  0x06 -> LightInactive
                                  0x07 -> SoundDB
                                  0x08 -> SoundDBA
                                  0x09 -> Custom
                                  0x0A -> Lowspeed
                                  0x0B -> Lowspeed9V
                                  0x0C -> NoOfSensorTypes
                                  _    -> throw $ PatternMatchFail "getInputValues"
                  sensormode' = case sensormode of
                                  0x00 -> RawMode
                                  0x20 -> BooleanMode
                                  0x40 -> TransitionCntMode
                                  0x60 -> PeriodCounterMode
                                  0x80 -> PctFullScaleMode
                                  0xA0 -> CelsiusMode
                                  0xC0 -> FahrenheitMode
                                  0xE0 -> AngleStepsMode
                                  -- two modes unnecessary?
                                  -- 0x1F -> SlopeMask
                                  -- 0xE0 -> ModeMask
                                  _    -> throw $ PatternMatchFail "getInputValues"
                  raw         = fromUWord . take 2 $ values
                  normalized  = fromUWord . take 2 . drop 2 $ values
                  scaled      = fromSWord . take 2 . drop 4 $ values
                  calibratedv = fromSWord . take 2 . drop 6 $ values
    _:_:e:_                                                       -> failNXT "getInputValues" e
    _                                                             -> fail "getInputValues"

-- Resets scaled value
resetInputScaledValue :: InputPort -> NXT ()
resetInputScaledValue input = do
  when debug $ liftIO . hPutStrLn stderr $ "resetinputscaledvalue"
  let send = [0x80, 0x08, fromIntegral . fromEnum $ input]
  sendData send

-- Writes a message
-- Message length is limited to 58 bytes per command
messageWrite :: Inbox -> String -> NXT ()
messageWrite = messageWrite' False

-- Writes a message, but also gets the confirmation
messageWriteConfirm :: Inbox -> String -> NXT ()
messageWriteConfirm = messageWrite' True

messageWrite' :: Bool -> Inbox -> String -> NXT ()
messageWrite' confirm inbox message
  | length message <= 58 = do
      when debug $ liftIO . hPutStrLn stderr $ "messagewrite"
      let message' = messageToData message
          send = [request, 0x09, fromIntegral . fromEnum $ inbox] ++ (toUByte . length $ message') ++ message'
      sendData send
      when confirm $ do
        receive <- receiveData
        case receive of
          [0x02, 0x09, 0x00] -> return ()
          [_, _, e]          -> failNXT "messageWrite" e
          _                  -> fail "messageWrite"
  | otherwise             = throw $ PatternMatchFail "messageWrite"
    where request = if confirm
                      then 0x00
                      else 0x80

-- Resets motor position
resetMotorPosition :: OutputPort -> MotorReset -> NXT ()
resetMotorPosition output reset = do
  when debug $ liftIO . hPutStrLn stderr $ "resetmotorposition"
  case reset of
    InternalPosition -> do
      mid <- getModuleID "Output.mod"
      writeIOMap (fromJust mid) ((fromEnum output) * 32 + 18) [0x08] -- Flags field is at offset 18, output block is 32 bytes long, UPDATE_RESET_COUNT is 0x08
    _                -> do
      let send = [0x80, 0x0A, fromIntegral . fromEnum $ output, fromIntegral . fromEnum $ reset]
      sendData send

-- Gets battery level (in mV)
getBatteryLevel :: NXT Voltage
getBatteryLevel = do
  when debug $ liftIO . hPutStrLn stderr $ "getbatterylevel"
  let send = [0x00, 0x0B]
  sendData send
  receive <- receiveData
  case receive of
    [0x02, 0x0B, 0x00, v1, v2] -> return $ fromUWord [v1, v2]
    _:_:e:_                    -> failNXT "getBatteryLevel" e
    _                          -> fail "getBatteryLevel"

-- Is battery rechargeable?
isBatteryRechargeable :: NXT Bool
isBatteryRechargeable = do
  when debug $ liftIO . hPutStrLn stderr $ "isbatteryrechargeable"
  mid <- getModuleID "Ui.mod"
  r <- readIOMap (fromJust mid) 35 1
  return $ (/=) 0 (head r)

-- Stops sound playback
stopSoundPlayback :: NXT ()
stopSoundPlayback = stopSoundPlayback' False

--Stops sound playback, but also gets the confirmation
stopSoundPlaybackConfirm :: NXT ()
stopSoundPlaybackConfirm = stopSoundPlayback' True

stopSoundPlayback' :: Bool -> NXT ()
stopSoundPlayback' confirm = do
  when debug $ liftIO . hPutStrLn stderr $ "stopsoundplayback"
  let send = [request, 0x0C]
  sendData send
  when confirm $ do
    receive <- receiveData
    case receive of
      [0x02, 0x0C, 0x00] -> return ()
      [_, _, e]          -> failNXT "stopSoundPlayback" e
      _                  -> fail "stopSoundPlayback"
    where request = if confirm
                      then 0x00
                      else 0x80

-- Sends a keep alive (turned on) packet
keepAlive :: NXT ()
keepAlive = do
  when debug $ liftIO . hPutStrLn stderr $ "keepalive"
  current <- liftIO $ getPOSIXTime
  modify (\s -> s { lastkeepalive = current })
  let send = [0x80, 0x0D]
  sendData send

-- Sends a keep alive (turned on) packet and gets current sleep time limit in milliseconds 
keepAliveDuration :: NXT Duration
keepAliveDuration = do
  when debug $ liftIO . hPutStrLn stderr $ "keepaliveduration"
  current <- liftIO $ getPOSIXTime
  modify (\s -> s { lastkeepalive = current })
  let send = [0x00, 0x0D]
  sendData send
  receive <- receiveData
  case receive of
    0x02:0x0D:0x00:limit -> do
      let l = (fromULong limit) % 1000 -- l is in milliseconds
      modify (\s -> s { sleeptime = l })
      return l
    _:_:e:_              -> failNXT "keepAliveDuration" e
    _                    -> fail "keepAliveDuration"

-- Gets number of available bytes to read
lowspeedGetStatus :: InputPort -> NXT Int
lowspeedGetStatus input = do
  when debug $ liftIO . hPutStrLn stderr $ "lowspeedgetstatus"
  let send = [0x00, 0x0E, fromIntegral . fromEnum $ input]
  sendData send
  receive <- receiveData
  case receive of
    [0x02, 0x0E, 0x00, bytes] -> return $ fromUByte [bytes]
    0x02:0x10:0x20:_          -> lowspeedGetStatus input -- pending communication transaction in progress, retrying
    _:_:e:_                   -> failNXT "lowSpeedGetStatus" e
    _                         -> fail "lowSpeedGetStatus"

-- Writes data
-- Rx data length must be specified in the write command since reading from the
-- device is done on a master-slave basis
-- Data length is limited to 16 bytes per command
lowspeedWrite :: InputPort -> RxDataLength -> TxData -> NXT ()
lowspeedWrite = lowspeedWrite' False

-- Writes data, but also gets the confirmation
lowspeedWriteConfirm :: InputPort -> RxDataLength -> TxData -> NXT ()
lowspeedWriteConfirm = lowspeedWrite' True

lowspeedWrite' :: Bool -> InputPort -> RxDataLength -> TxData -> NXT ()
lowspeedWrite' confirm input rx txdata
  | length txdata <= 16 && rx <= 16 = do
      when debug $ liftIO . hPutStrLn stderr $ "lowspeedwrite"
      let send = [request, 0x0F, fromIntegral . fromEnum $ input] ++ (toUByte . length $ txdata) ++ (toUByte rx) ++ txdata
      sendData send
      when confirm $ do
        receive <- receiveData
        case receive of
          [0x02, 0x0F, 0x00] -> return ()
          [_, _, e]          -> failNXT "lowspeedWrite" e
          _                  -> fail "lowspeedWrite"
  | otherwise                       = throw $ PatternMatchFail "lowspeedWrite"
    where request = if confirm
                      then 0x00
                      else 0x80

-- Reads data
-- The protocol does not support variable-length return packages so the response
-- always contains 16 data bytes with invalid data padded with zeros
-- Data length is limited to 16 bytes per command
lowspeedRead :: InputPort -> NXT RxData
lowspeedRead input = do
  when debug $ liftIO . hPutStrLn stderr $ "lowspeedread"
  let send = [0x00, 0x10, fromIntegral . fromEnum $ input]
  sendData send
  receive <- receiveData
  case receive of
    0x02:0x10:0x00:rx:rxdata
      | length rxdata == 16 && rx <= 16 -> return $ take (fromUByte [rx]) rxdata
    0x02:0x10:0x20:_                    -> lowspeedRead input -- pending communication transaction in progress, retrying
    _:_:e:_                             -> failNXT "lowSpeedRead" e
    _                                   -> fail "lowSpeedRead"

-- Gets current program name
getCurrentProgramName :: NXT (Maybe String)
getCurrentProgramName = do
  when debug $ liftIO . hPutStrLn stderr $ "getcurrentprogramname"
  let send = [0x00, 0x11]
  sendData send
  receive <- receiveData
  case receive of
    0x02:0x11:0x00:filename | length filename == 20 -> return $ Just $ dataToString0 filename
    0x02:0x11:0xEC:_                                -> return Nothing
    _:_:e:_                                         -> failNXT "getCurrentProgramName" e
    _                                               -> fail "getCurrentProgramName"

-- Reads a message
messageRead :: RemoteInbox -> Bool -> NXT String
messageRead inbox remove = do
  when debug $ liftIO . hPutStrLn stderr $ "messageRead"
  let inbox' = fromIntegral . fromEnum $ inbox
      send = [0x00, 0x13, inbox', fromIntegral . fromEnum $ Inbox0, fromIntegral . fromEnum $ remove] -- local inbox number does not matter for PC, it is used only when master NXT reads from slave NXT
  sendData send
  receive <- receiveData
  case receive of
    0x02:0x13:0x00:inbox'':size:message
      | inbox'' == inbox' && length message == 59 && size <= 59 -> return $ dataToString0 message
    _:_:e:_                                                     -> failNXT "messageRead" e
    _                                                           -> fail "messageRead"

-- Stops all NXT activities: stops motors and disables sensors
stopEverything :: NXT ()
stopEverything = do
  when debug $ liftIO . hPutStrLn stderr $ "stopeverything"
  mapM_ stopMotor [A ..]
  mapM_ stopSensor [One ..]
    where stopMotor x = setOutputState x 0 [] RegulationModeIdle 0 MotorRunStateIdle 0
          stopSensor x = setInputMode x NoSensor RawMode

shutdown :: NXT ()
shutdown = do
  when debug $ liftIO . hPutStrLn stderr $ "shutdown"
  mid <- getModuleID "IOCtrl.mod"
  writeIOMap (fromJust mid) 0 [0x00, 0x5A]

-- Opens a file for writing a linked list of flash sectors
openWrite :: FileName -> FileSize -> NXT FileHandle
openWrite filename filesize = do
  when debug $ liftIO . hPutStrLn stderr $ "openwrite"
  let send = [0x01, 0x81] ++ (nameToData filename) ++ (toULong filesize)
  sendData send
  receive <- receiveData
  case receive of
    [0x02, 0x81, 0x00, h] -> return $ fromUByte [h]
    _:_:e:_               -> failNXT "openWrite" e
    _                     -> fail "openWrite"

-- Opens a file for writing a linear contiguous block of flash memory (required for user programs and certain data files)
openWriteLinear :: FileName -> FileSize -> NXT FileHandle
openWriteLinear filename filesize = do
  when debug $ liftIO . hPutStrLn stderr $ "openwritelinear"
  let send = [0x01, 0x89] ++ (nameToData filename) ++ (toULong filesize)
  sendData send
  receive <- receiveData
  case receive of
    [0x02, 0x89, 0x00, h] -> return $ fromUByte [h]
    _:_:e:_               -> failNXT "openWriteLinear" e
    _                     -> fail "openWriteLinear"

-- Writes data to a file
-- Data length is limited to 61 bytes per command
write :: FileHandle -> FileData -> NXT ()
write = write' False

-- Writes data to a file, but also gets the confirmation
writeConfirm :: FileHandle -> FileData -> NXT ()
writeConfirm = write' True

write' :: Bool -> FileHandle -> FileData -> NXT ()
write' confirm filehandle filedata
  | length filedata <= 61 = do
      when debug $ liftIO . hPutStrLn stderr $ "write"
      let send = [request, 0x83] ++ (toUByte filehandle) ++ filedata
      sendData send
      when confirm $ do
        receive <- receiveData
        case receive of
          [0x02, 0x83, 0x00, h, bw1, bw2]
            | fromUByte [h] == filehandle && length filedata == fromUWord [bw1, bw2] -> return ()
          _:_:e:_                                                                    -> failNXT "write" e
          _                                                                          -> fail "write"
  | otherwise             = throw $ PatternMatchFail "write"
    where request = if confirm
                      then 0x01
                      else 0x81
-- Closes a file
close :: FileHandle -> NXT ()
close = close' False

-- Closes a file, but also gets the confirmation
closeConfirm :: FileHandle -> NXT ()
closeConfirm = close' True

close' :: Bool -> FileHandle -> NXT ()
close' confirm filehandle = do
  when debug $ liftIO . hPutStrLn stderr $ "close"
  let send = [request, 0x84] ++ (toUByte filehandle)
  sendData send
  when confirm $ do
    receive <- receiveData
    case receive of
      [0x02, 0x84, 0x00, h]
        | fromUByte [h] == filehandle -> return ()
      _:_:e:_                         -> failNXT "close" e
      _                               -> fail "close"
    where request = if confirm
                      then 0x01
                      else 0x81

-- Deletes a file
delete :: FileName -> NXT ()
delete = delete' False False

-- Deletes a file, but also gets the confirmation
deleteConfirm :: FileName -> NXT ()
deleteConfirm = delete' True False

-- Deletes a file, but requires file existence
deleteExisting :: FileName -> NXT ()
deleteExisting = delete' True True

delete' :: Bool -> Bool -> FileName -> NXT ()
delete' confirm existence filename = do
  when debug $ liftIO . hPutStrLn stderr $ "delete"
  let send = [request, 0x85] ++ (nameToData filename)
  sendData send
  when confirm $ do
    receive <- receiveData
    case receive of
      0x02:0x85:0x00:f
        | dataToString0 f == filename -> return ()
      0x02:0x85:0x87:_                -> if existence
                                           then failNXT "delete" 0x87
                                           else return ()
      _:_:e:_                         -> failNXT "delete" e
      _                               -> fail "delete"
    where request = if confirm
                      then 0x01
                      else 0x81

-- Requests information about the first module matching given module name (which can be a wild card)
requestFirstModule :: ModuleName -> NXT (ModuleHandle, Maybe ModuleInfo)
requestFirstModule modulename = do
  when debug $ liftIO . hPutStrLn stderr $ "requestfirstmodule"
  let send = [0x01, 0x90] ++ (nameToData modulename)
  sendData send
  receive <- receiveData
  case receive of
    0x02:0x90:0x00:h:values
      | length values == 30 -> return (fromUByte [h], Just $ ModuleInfo name moduleid size iomapsize)
                                 where name      = dataToString0 . take 20 $ values
                                       moduleid  = fromULong . take 4 . drop 20 $ values
                                       size      = fromULong . take 4 . drop 24 $ values
                                       iomapsize = fromUWord . take 2 . drop 28 $ values
    0x02:0x90:0x90:h:_      -> return (fromUByte [h], Nothing) -- module not found
    _:_:e:_                 -> failNXT "requestFirstModule" e
    _                       -> fail "requestFirstModule"

-- Requests information about the next module matching previously requested module name (which can be a wild card)
requestNextModule :: ModuleHandle -> NXT (ModuleHandle, Maybe ModuleInfo)
requestNextModule modulehandle = do
  when debug $ liftIO . hPutStrLn stderr $ "requestnextmodule"
  let send = [0x01, 0x91] ++ (toUByte modulehandle)
  sendData send
  receive <- receiveData
  case receive of
    0x02:0x91:0x00:h:values
      | length values == 30 -> return (fromUByte [h], Just $ ModuleInfo name moduleid size iomapsize)
                                 where name      = dataToString0 . take 20 $ values
                                       moduleid  = fromULong . take 4 . drop 20 $ values
                                       size      = fromULong . take 4 . drop 24 $ values
                                       iomapsize = fromUWord . take 2 . drop 28 $ values
    0x02:0x91:0x90:h:_      -> return (fromUByte [h], Nothing) -- module not found
    _:_:e:_                 -> failNXT "requestNextModule" e
    _                       -> fail "requestNextModule"

-- Closes previously requested module information
closeModuleHandle :: ModuleHandle -> NXT ()
closeModuleHandle = closeModuleHandle' False

-- Closes previously requested module information, but also gets the confirmation
closeModuleHandleConfirm :: ModuleHandle -> NXT ()
closeModuleHandleConfirm = closeModuleHandle' True

closeModuleHandle' :: Bool -> ModuleHandle -> NXT ()
closeModuleHandle' confirm modulehandle = do
  when debug $ liftIO . hPutStrLn stderr $ "closemodulehandle"
  let send = [request, 0x92] ++ (toUByte modulehandle)
  sendData send
  when confirm $ do
    receive <- receiveData
    case receive of
      [0x02, 0x92, 0x00, h]
        | fromUByte [h] == modulehandle -> return ()
      _:_:e:_                           -> failNXT "closeModuleHandle" e
      _                                 -> fail "closeModuleHandle"
    where request = if confirm
                      then 0x01
                      else 0x81

-- Gets information about all modules matching given module name (which can be a wild card)
listModules :: ModuleName -> NXT [ModuleInfo]
listModules modulename = do
  (h, first) <- requestFirstModule modulename
  case first of
    Nothing     -> do
      closeModuleHandle h
      return []
    Just first' -> do
      (h', other) <- next h
      closeModuleHandle h'
      return (first':other)
    where next h = do
            (h', mi) <- requestNextModule h
            case mi of
              Just mi' -> do
                (h'',mi'') <- next h'
                return (h'',mi':mi'')
              Nothing  -> return (h', [])

-- Reads IO map data of a module
-- At most 119 bytes can be read at a time
readIOMap :: ModuleID -> IOMapOffset -> IOMapLength -> NXT IOMapData
readIOMap moduleid offset len
  | offset >= 0 && len <= 119 = do
      when debug $ liftIO . hPutStrLn stderr $ "readiomap"
      let send = [0x01, 0x94] ++ (toULong moduleid) ++ (toUWord offset) ++ (toUWord len)
      sendData send
      receive <- receiveData
      case receive of
        0x02:0x94:0x00:mid1:mid2:mid3:mid4:r1:r2:values
          | fromULong [mid1, mid2, mid3, mid4] == moduleid && fromUWord [r1, r2] == len -> return values
        _:_:e:_                                                                         -> failNXT "readIOMap" e
        _                                                                               -> fail "readIOMap"
  | otherwise                 = throw $ PatternMatchFail "readIOMap"

-- Writes data to an IO map of a module
-- Data length is limited to 54 bytes per command
writeIOMap :: ModuleID -> IOMapOffset -> IOMapData -> NXT ()
writeIOMap = writeIOMap' False

-- Writes data to an IO map of a module, but also gets the confirmation
writeIOMapConfirm :: ModuleID -> IOMapOffset -> IOMapData -> NXT ()
writeIOMapConfirm = writeIOMap' True

writeIOMap' :: Bool -> ModuleID -> IOMapOffset -> IOMapData -> NXT ()
writeIOMap' confirm moduleid offset mapdata
  | offset >= 0 && length mapdata <= 54 = do
      when debug $ liftIO . hPutStrLn stderr $ "writeiomap"
      let send = [request, 0x95] ++ (toULong moduleid) ++ (toUWord offset) ++ (toUWord $ length mapdata) ++ mapdata
      sendData send
      when confirm $ do
        receive <- receiveData
        case receive of
          [0x02, 0x95, 0x00, mid1, mid2, mid3, mid4, w1, w2]
            | fromULong [mid1, mid2, mid3, mid4] == moduleid && fromUWord [w1, w2] == length mapdata -> return ()
          _:_:e:_                                                                                    -> failNXT "writeIOMap" e
          _                                                                                          -> fail "writeIOMap"
  | otherwise                           = throw $ PatternMatchFail "writeIOMap"
    where request = if confirm
                      then 0x01
                      else 0x81

-- Gets an ID of a module matching given module name
-- Hopefully retrieves it from a cache of previous requests
getModuleID :: ModuleName -> NXT (Maybe ModuleID)
getModuleID modulename = do
  if '*' `elem` modulename
    then return Nothing -- we do not allow wild cards
    else do
      mods <- gets modules
      let modulename' = map toLower modulename
      case modulename' `lookup` mods of
        Just (ModuleInfo _ mid _ _) -> return $ Just mid
        Nothing                     -> do
          (h, mi) <- requestFirstModule modulename'
          closeModuleHandle h
          case mi of
            Just mi'@(ModuleInfo _ mid _ _) -> do
              modify (\s -> s { modules = (modulename', mi'):mods })
              return $ Just mid
            Nothing                         -> return Nothing
