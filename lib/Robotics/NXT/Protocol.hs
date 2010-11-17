{-# LANGUAGE ForeignFunctionInterface #-}
{-# CFILES ffi/initserial.c #-}

module Robotics.NXT.Protocol (
  -- * Initialization
  withNXT,
  defaultDevice,

  -- * Motors
  setOutputState,
  setOutputStateConfirm,
  getOutputState,
  resetMotorPosition,

  -- * Sensors
  setInputMode,
  setInputModeConfirm,
  getInputValues,
  resetInputScaledValue,

  -- * Miscellaneous
  getVersion,
  getDeviceInfo,
  getBatteryLevel,
  isBatteryRechargeable,
  keepAlive,
  keepAliveConfirm,
  getSleepTimeout,
  getLastKeepAliveTime,
  stopEverything,
  shutdown,

  -- * Remote Programs
  -- | It is possible to remotely run and control (with messages) programs on the NXT brick. Those here are low-level functions
  -- but check also high-level "Robotics.NXT.Remote" and "Robotics.NXT.MotorControl" modules.
  startProgram,
  startProgramConfirm,
  stopProgram,
  stopProgramConfirm,
  stopProgramExisting,
  ensureStartProgram,
  getCurrentProgramName,

  -- * Messages
  -- | It is possible to control programs on the NXT brick with messages. Those here are low-level functions
  -- but check also high-level "Robotics.NXT.Remote" and "Robotics.NXT.MotorControl" modules.
  messageWrite,
  messageWriteConfirm,
  messageRead,
  maybeMessageRead,
  ensureMessageRead,

  -- * Sounds
  playSoundFile,
  playSoundFileConfirm,
  playTone,
  stopSoundPlayback,
  stopSoundPlaybackConfirm,

  -- * Low Speed (I2C)
  -- | With those low-level functions it is possible to communicate with digital sensors attached to the NXT brick. But check
  -- also high-level "Robotics.NXT.Sensor.Ultrasonic" and "Robotics.NXT.Sensor.Compass" modules.
  lowspeedGetStatus,
  lowspeedWrite,
  lowspeedWriteConfirm,
  lowspeedRead,

  -- * Filesystem
  openWrite,
  openWriteLinear,
  write,
  writeConfirm,
  close,
  closeConfirm,
  delete,
  deleteConfirm,
  deleteExisting,

  -- * IO Map
  -- | Interface to NXT firmware is based on internal IO map interface. All commands are in fact just pretty wrappers to this
  -- interface, but it is possible to use it directly and thus gain some additional possibilities which are not
  -- available otherwise (some of those are already wrapped in this interface's additional functions and feel free to suggest
  -- more if you need them).
  getModuleID,
  listModules,
  requestFirstModule,
  requestNextModule,
  closeModuleHandle,
  closeModuleHandleConfirm,
  readIOMap,
  writeIOMap,
  writeIOMapConfirm,
  
  -- * Internals
  -- | Be careful when using those functions as you have to assure your program is well-behaved: you should see 'NXTInternals' as a
  -- token you have to pass around in order, not reusing or copying values. They are exposed so that you can decouple initalization,
  -- execution and termination phase. If you do not need that use 'withNXT'.
  --
  -- For example, using 'bracket' is not the best way to combine them together as token returned from 'initialize' in \"acquire resource\"
  -- phase is reused in \"release resource\" phase even if it was otherwise used in-between. Really use 'withNXT' for that.
  initialize,
  terminate,
  runNXT,
  execNXT
) where

import qualified Data.ByteString as B
import Control.Exception
import Control.Monad.State
import Data.Bits
import Data.Char
import Data.List hiding (delete)
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

import Robotics.NXT.Data
import Robotics.NXT.Errors
import Robotics.NXT.Types
import Robotics.NXT.Internals

-- Described in Lego Mindstorms NXT Bluetooth Developer Kit:
--  Appendix 1 - Communication protocol
--  Appendix 2 - Direct commands

-- TODO: All functions which requests ModuleInfo could populate module ID cache along the way
-- TODO: Add an optional warning if direction of communication changes
-- TODO: Implement all missing "confirm" versions

-- Foreign function call for C function which initialize serial port device on POSIX systems
foreign import ccall unsafe "initSerialPort" initSerialPort' :: Fd -> IO CInt

initSerialPort :: Fd -> IO ()
initSerialPort fd = throwErrnoIfMinus1_ "initSerialPort" $ initSerialPort' fd

{-|
Default Bluetooth serial device filename for current operating system. Currently always @\/dev\/rfcomm0@.
-}
defaultDevice :: FilePath
defaultDevice = "/dev/rfcomm0"

debug :: Bool
debug = False

{-|
Opens and intializes a Bluetooth serial device communication.
-}
initialize :: FilePath -> IO NXTInternals
initialize device = do
  -- we have to block signals from interrupting openFd system call (fixed in GHC versions after 6.12.1)
  let signals = foldl (flip addSignal) emptySignalSet [virtualTimerExpired]
  blockSignals signals
  fd <- openFd device ReadWrite Nothing OpenFileFlags { append = False, noctty = True, exclusive = False, nonBlock = True, trunc = False }
  unblockSignals signals
  initSerialPort fd
  h <- fdToHandle fd
  hSetBuffering h NoBuffering
  when debug $ hPutStrLn stderr "initialized"
  return $ NXTInternals h Nothing [] Nothing Nothing

{-|
Stops all NXT activities (by calling 'stopEverything') and closes the Bluetooth serial device communication. 'NXTInternals' token must not
be used after that anymore.
-}
terminate :: NXTInternals -> IO ()
terminate i = do
  i' <- execNXT stopEverything i
  let h = nxthandle i'
  hClose h
  when debug $ hPutStrLn stderr "terminated"

-- TODO: Change to mask/restore in GHC 7.0
{-|
Function which initializes and terminates Bluetooth connection to the NXT brick (using 'initialize' and 'terminate') and in-between
runs given computation. It terminates Bluetooth connection on an exception, too, rethrowing it afterwards.
-}
withNXT :: FilePath -> NXT a -> IO a
withNXT device action = block $ do
  i <- initialize device
  (r, i') <- unblock (runNXT action i) `onException` terminate i
  terminate i'
  return r

-- Main function for sending data to NXT
-- It calculates the length and prepends it to the message
sendData :: [Word8] -> NXT ()
sendData message = do
  h <- getsNXT nxthandle
  let len = toUWord . length $ message
      packet = len ++ message
  liftIO . B.hPut h . B.pack $ packet
  when debug $ liftIO . hPutStrLn stderr $ "sent: " ++ show packet

-- Main function for receiving data from NXT
receiveData :: NXT [Word8]
receiveData = do
  h <- getsNXT nxthandle
  len <- liftIO $ B.hGet h 2
  let len' = fromUWord . B.unpack $ len
  packet <- liftIO $ B.hGet h len'
  let unpacket = B.unpack packet
  when debug $ liftIO . hPutStrLn stderr $ "received: " ++ show unpacket
  return unpacket

{-|
Gets firmware and protocol versions of the NXT brick.
-}
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
    _:_:e:_                                            -> liftIO $ failNXT "getVersion" e
    _                                                  -> liftIO $ failNXT' "getVersion"

{-|
Gets device (the NXT brick) information: name, Bluetooth 48 bit address in the string format, strength of Bluetooth signal (not implemented in
current NXT firmware versions, use 'bluetoothRSSI' or 'bluetoothLinkQuality' as an alternative), free space on flash.
-}
getDeviceInfo :: NXT DeviceInfo
getDeviceInfo = do
  when debug $ liftIO . hPutStrLn stderr $ "getdeviceinfo"
  let send = [0x01, 0x9B]
  sendData send
  receive <- receiveData
  case receive of
    0x02:0x9B:0x00:info | length info == 30 -> do
      modifyNXT (\s -> s { address = Just btaddress }) -- we cache it
      return $ DeviceInfo name' btaddress btstrength flashfree
        where (name, info') = splitAt 15 info
              name' = dataToString0 name
              btaddress = map toUpper . intercalate ":" . map (printf "%02x") . take 6 $ info'
              -- 7th byte not used?
              btstrength = fromULong . take 4 . drop 7 $ info'
              flashfree = fromULong . take 4 . drop 11 $ info'
    _:_:e:_               -> liftIO $ failNXT "getDeviceInfo" e
    _                     -> liftIO $ failNXT' "getDeviceInfo"

{-|
Starts a given program on the NXT brick.
-}
startProgram :: FileName -> NXT ()
startProgram = startProgram' False

{-|
Same as 'startProgram' but also request a confirmation. Useful to assure the command was really accepted, but this does not assure
that the program has really started successfully (especially not that it is already running when the confirmation is received).
Use 'ensureStartProgram' for that. In a case of an error it throws a 'NXTException'.

Confirmation requires a change of the direction of NXT Bluetooth communication which takes around 30 ms.
-}
startProgramConfirm :: FileName -> NXT ()
startProgramConfirm = startProgram' True

startProgram' :: Bool -> FileName -> NXT ()
startProgram' confirm filename = do
  when debug $ liftIO . hPutStrLn stderr $ "startprogram"
  let send = [request confirm, 0x00] ++ nameToData filename
  sendData send
  when confirm $ do
    receive <- receiveData
    case receive of
      [0x02, 0x00, 0x00] -> return ()
      [_, _, e]          -> liftIO $ failNXT "startProgram" e
      _                  -> liftIO $ failNXT' "startProgram"

{-|
Stops a currently running program.
-}
stopProgram :: NXT ()
stopProgram = stopProgram' False False

{-|
Same as 'stopProgram' but also request a confirmation. Useful to assure the command was really accepted. In a case of an error it
throws a 'NXTException'.

Confirmation requires a change of the direction of NXT Bluetooth communication which takes around 30 ms. 
-}
stopProgramConfirm :: NXT ()
stopProgramConfirm = stopProgram' True False

{-|
Same as 'stopProgramConfirm' but it also requires that the program was really running. It throws a 'NXTException' otherwise.
-}
stopProgramExisting :: NXT ()
stopProgramExisting = stopProgram' True True

stopProgram' :: Bool -> Bool -> NXT ()
stopProgram' confirm running = do
  when debug $ liftIO . hPutStrLn stderr $ "stopprogram"
  let send = [request confirm, 0x01]
  sendData send
  when confirm $ do
    receive <- receiveData
    case receive of
      [0x02, 0x01, 0x00] -> return ()
      [0x02, 0x01, 0xEC] -> when running $ liftIO $ failNXT "stopProgram" 0xEC
      [_, _, e]          -> liftIO $ failNXT "stopProgram" e
      _                  -> liftIO $ failNXT' "stopProgram"

-- TODO: Could probably loop infinitely in some strange situation? Some timeout could be useful?
{-|
Helper function which first ensures that no other program is running and then ensures that a given program is really running before
it returns.
-}
ensureStartProgram :: FileName -> NXT ()
ensureStartProgram filename = do
  stopAndWait
  startAndWait
    where stopAndWait = do
            stopProgramConfirm
            name <- getCurrentProgramName
            unless (isNothing name) stopAndWait
          startAndWait = do
            startProgramConfirm filename
            name <- getCurrentProgramName
            unless (isJust name) startAndWait

{-|
Plays a given sound file.
-}
playSoundFile :: LoopPlayback -> FileName -> NXT ()
playSoundFile = playSoundFile' False

{-|
Same as 'playSoundFile' but also request a confirmation. Useful to assure the command was really accepted. In a case of an error it
throws a 'NXTException'.

Confirmation requires a change of the direction of NXT Bluetooth communication which takes around 30 ms. 
-}
playSoundFileConfirm :: LoopPlayback -> FileName -> NXT ()
playSoundFileConfirm = playSoundFile' True

playSoundFile' :: Bool -> LoopPlayback -> FileName -> NXT ()
playSoundFile' confirm loop filename = do
  when debug $ liftIO . hPutStrLn stderr $ "playsoundfile"
  let send = [request confirm, 0x02, fromIntegral . fromEnum $ loop] ++ nameToData filename
  sendData send
  when confirm $ do
    receive <- receiveData
    case receive of
      [0x02, 0x02, 0x00] -> return ()
      [_, _, e]          -> liftIO $ failNXT "playSoundFile" e
      _                  -> liftIO $ failNXT' "playSoundFile"

{-|
Plays a tone with a given frequency (in hertz) for a given duration (in seconds).
-}
playTone :: Frequency -> Duration -> NXT ()
playTone frequency duration = do
  when debug $ liftIO . hPutStrLn stderr $ "playtone"
  let send = [0x80, 0x03] ++ toUWord frequency ++ toUWord (toMilliseconds duration)
  sendData send
    where toMilliseconds :: Duration -> Integer -- duration is in seconds, but NXT requires milliseconds
          toMilliseconds d = floor (d * 1000)

{-|
Sets output port (motor) state. This is the main function for controlling a motor.
-}
setOutputState :: OutputPort -> OutputPower -> [OutputMode] -> RegulationMode -> TurnRatio -> RunState -> TachoLimit -> NXT ()
setOutputState = setOutputState' False

{-|
Same as 'setOutputState' but also request a confirmation. Useful to assure the command was really accepted. In a case of an error
it throws a 'NXTException'.

Confirmation requires a change of the direction of NXT Bluetooth communication which takes around 30 ms.
-}
setOutputStateConfirm :: OutputPort -> OutputPower -> [OutputMode] -> RegulationMode -> TurnRatio -> RunState -> TachoLimit -> NXT ()
setOutputStateConfirm = setOutputState' True

setOutputState' :: Bool -> OutputPort -> OutputPower -> [OutputMode] -> RegulationMode -> TurnRatio -> RunState -> TachoLimit -> NXT ()
setOutputState' confirm output power mode regulation turn runstate tacholimit
  | power >= -100 && power <= 100 && turn >= -100 && turn <= 100 = do
      when debug $ liftIO . hPutStrLn stderr $ "setoutputstate"
      let send = [request confirm, 0x04, fromIntegral . fromEnum $ output] ++ toSByte power ++ [modebyte, regulation'] ++ toSByte turn ++ [runstate'] ++ toULong tacholimit
      sendData send
      when confirm $ do
        receive <- receiveData
        case receive of
          [0x02, 0x04, 0x00] -> return ()
          [_, _, e]          -> liftIO $ failNXT "setOutputState" e
          _                  -> liftIO $ failNXT' "setOutputState"
  | otherwise                                                    = liftIO . throwIO $ PatternMatchFail "setOutputState"
    where modebyte    = foldl (.|.) 0x00 . map convmode $ mode
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

{-|
Gets output port (motor) current state. In additional to values used with 'setOutputState' also 'TachoCount', 'BlockTachoCount'
and 'RotationCount' values are available which tell you current position of a motor.
-}
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
                  motoron = [MotorOn | testBit modebyte 0]
                  brake = [Brake | testBit modebyte 1]
                  regulated = [Regulated | testBit modebyte 2]
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
    _:_:e:_                                                         -> liftIO $ failNXT "getOutputState" e
    _                                                               -> liftIO $ failNXT' "getOutputState"

{-|
Sets input port (sensor) type and mode.
-}
setInputMode :: InputPort -> SensorType -> SensorMode -> NXT ()
setInputMode = setInputMode' False

{-|
Same as 'setInputMode' but also request a confirmation. Useful to assure the command was really accepted. In a case of an error it throws a 'NXTException'.

Confirmation requires a change of the direction of NXT Bluetooth communication which takes around 30 ms. 
-}
setInputModeConfirm :: InputPort -> SensorType -> SensorMode -> NXT ()
setInputModeConfirm = setInputMode' True

setInputMode' :: Bool -> InputPort -> SensorType -> SensorMode -> NXT ()
setInputMode' confirm input sensortype sensormode = do
  when debug $ liftIO . hPutStrLn stderr $ "setinputmode"
  let send = [request confirm, 0x05, fromIntegral . fromEnum $ input, sensortype', sensormode']
  sendData send
  when confirm $ do
    receive <- receiveData
    case receive of
      [0x02, 0x05, 0x00] -> return ()
      [_, _, e]          -> liftIO $ failNXT "setInputMode" e
      _                  -> liftIO $ failNXT' "setInputMode"
    where sensortype' = case sensortype of
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

{-|
Gets input port (sensor) values. This is the main function for reading a sensor.
-}
getInputValues :: InputPort -> NXT InputValue
getInputValues input = do
  when debug $ liftIO . hPutStrLn stderr $ "getinputvalues"
  let send = [0x00, 0x07, fromIntegral . fromEnum $ input]
  sendData send
  receive <- receiveData
  case receive of
    0x02:0x07:0x00:port:valid:calibrated:sensortype:sensormode:values
      | length values == 8 && fromEnum input == fromIntegral port ->
          return $ InputValue input valid' calibrated' sensortype' sensormode' raw normalized scaled calibratedv
            where valid'      = valid /= 0x00
                  calibrated' = calibrated /= 0x00
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
    _:_:e:_                                                       -> liftIO $ failNXT "getInputValues" e
    _                                                             -> liftIO $ failNXT' "getInputValues"

{-|
Resets input port (sensor) scaled value.
-}
resetInputScaledValue :: InputPort -> NXT ()
resetInputScaledValue input = do
  when debug $ liftIO . hPutStrLn stderr $ "resetinputscaledvalue"
  let send = [0x80, 0x08, fromIntegral . fromEnum $ input]
  sendData send

{-|
Writes a message to the given inbox queue of the running remote program. A message length is limited to 58 characters/bytes. A queue
is limited to 5 messages.
-}
messageWrite :: Inbox -> String -> NXT ()
messageWrite = messageWrite' False

{-|
Same as 'messageWrite' but also request a confirmation. Useful to assure the command was really accepted. In a case of an error it
throws a 'NXTException'.

Confirmation requires a change of the direction of NXT Bluetooth communication which takes around 30 ms.
-}
messageWriteConfirm :: Inbox -> String -> NXT ()
messageWriteConfirm = messageWrite' True

messageWrite' :: Bool -> Inbox -> String -> NXT ()
messageWrite' confirm inbox message
  | length message <= 58 = do
      when debug $ liftIO . hPutStrLn stderr $ "messagewrite"
      let message' = messageToData message
          send = [request confirm, 0x09, fromIntegral . fromEnum $ inbox] ++ (toUByte . length $ message') ++ message'
      sendData send
      when confirm $ do
        receive <- receiveData
        case receive of
          [0x02, 0x09, 0x00] -> return ()
          [_, _, e]          -> liftIO $ failNXT "messageWrite" e
          _                  -> liftIO $ failNXT' "messageWrite"
  | otherwise             = liftIO . throwIO $ PatternMatchFail "messageWrite"

{-|
Resets one of three position counters for a given output port.
-}
resetMotorPosition :: OutputPort -> MotorReset -> NXT ()
resetMotorPosition output reset = do
  when debug $ liftIO . hPutStrLn stderr $ "resetmotorposition"
  case reset of
    InternalPosition -> do
      mid <- getModuleID "Output.mod"
      writeIOMap (fromJust mid) (fromEnum output * 32 + 18) [0x08] -- flags field is at offset 18, output block is 32 bytes long, UPDATE_RESET_COUNT is 0x08
    _                -> do
      let send = [0x80, 0x0A, fromIntegral . fromEnum $ output, fromIntegral . fromEnum $ reset]
      sendData send

{-|
Gets current battery level (in volts).
-}
getBatteryLevel :: NXT Voltage
getBatteryLevel = do
  when debug $ liftIO . hPutStrLn stderr $ "getbatterylevel"
  let send = [0x00, 0x0B]
  sendData send
  receive <- receiveData
  case receive of
    [0x02, 0x0B, 0x00, v1, v2] -> return $ fromUWord [v1, v2] % 1000 -- voltage is in millivolts
    _:_:e:_                    -> liftIO $ failNXT "getBatteryLevel" e
    _                          -> liftIO $ failNXT' "getBatteryLevel"

{-|
Is battery used in the NXT brick rechargeable?
-}
isBatteryRechargeable :: NXT Bool
isBatteryRechargeable = do
  when debug $ liftIO . hPutStrLn stderr $ "isbatteryrechargeable"
  mid <- getModuleID "Ui.mod"
  r <- readIOMap (fromJust mid) 35 1
  return $ (/=) 0 (head r)

{-|
Stops current sound file playback.
-}
stopSoundPlayback :: NXT ()
stopSoundPlayback = stopSoundPlayback' False

{-|
Same as 'stopSoundPlayback' but also request a confirmation. Useful to assure the command was really accepted. In a case of an
error it throws a 'NXTException'.

Confirmation requires a change of the direction of NXT Bluetooth communication which takes around 30 ms. 
-}
stopSoundPlaybackConfirm :: NXT ()
stopSoundPlaybackConfirm = stopSoundPlayback' True

stopSoundPlayback' :: Bool -> NXT ()
stopSoundPlayback' confirm = do
  when debug $ liftIO . hPutStrLn stderr $ "stopsoundplayback"
  let send = [request confirm, 0x0C]
  sendData send
  when confirm $ do
    receive <- receiveData
    case receive of
      [0x02, 0x0C, 0x00] -> return ()
      [_, _, e]          -> liftIO $ failNXT "stopSoundPlayback" e
      _                  -> liftIO $ failNXT' "stopSoundPlayback"

{-|
Sends a keep alive (turned on) packet. It prevents the NXT brick from automatically powering off. Other commands do not prevent that
from hapenning so it is useful to send this packet from time to time if you want to prevent powering off.
-}
keepAlive :: NXT ()
keepAlive = keepAlive' False >> return ()

{-|
Same as 'keepAlive' but also request a confirmation. Useful to assure the command was really accepted. In a case of an error it
throws a 'NXTException'.

Confirmation requires a change of the direction of NXT Bluetooth communication which takes around 30 ms. 
-}
keepAliveConfirm :: NXT ()
keepAliveConfirm = keepAlive' True >> return ()

keepAlive' :: Bool -> NXT Duration
keepAlive' confirm = do
  when debug $ liftIO . hPutStrLn stderr $ "keepalive"
  current <- liftIO getPOSIXTime
  modifyNXT (\s -> s { lastkeepalive = Just current })
  let send = [0x00, 0x0D]
  sendData send
  if confirm
    then do
      receive <- receiveData
      case receive of
        0x02:0x0D:0x00:limit -> do
          let l = fromRational $ fromULong limit % 1000 -- limit is in milliseconds
          modifyNXT (\s -> s { sleeptime = Just l })
          return l
        _:_:e:_              -> liftIO $ failNXT "keepAlive" e
        _                    -> liftIO $ failNXT' "keepAlive"
    else return 0

{-|
Gets current sleep timeout setting (in seconds) after which the NXT brick automatically powers off if
not prevented with a keep alive packet (use 'keepAlive' to send one). This setting is cached.
-}
getSleepTimeout :: NXT Duration
getSleepTimeout = do
  sleep <- getsNXT sleeptime
  case sleep of
    Just s  -> return s
    Nothing -> keepAlive' True

{-|
When was a last keep alive packet send?
-}
getLastKeepAliveTime :: NXT (Maybe POSIXTime)
getLastKeepAliveTime = getsNXT lastkeepalive

{-|
Gets number of bytes available to read.
-}
lowspeedGetStatus :: InputPort -> NXT Int
lowspeedGetStatus input = do
  when debug $ liftIO . hPutStrLn stderr $ "lowspeedgetstatus"
  let send = [0x00, 0x0E, fromIntegral . fromEnum $ input]
  sendData send
  receive <- receiveData
  case receive of
    [0x02, 0x0E, 0x00, bytes] -> return $ fromUByte [bytes]
    0x02:0x10:0x20:_          -> lowspeedGetStatus input -- pending communication transaction in progress, retrying
    _:_:e:_                   -> liftIO $ failNXT "lowSpeedGetStatus" e
    _                         -> liftIO $ failNXT' "lowSpeedGetStatus"

{-|
Writes data. At most 16 bytes can be written at a time.

Reply data length must be specified in the write command since reading from the device is done on a master-slave basis.
-}
lowspeedWrite :: InputPort -> RxDataLength -> TxData -> NXT ()
lowspeedWrite = lowspeedWrite' False

{-|
Same as 'lowspeedWrite' but also request a confirmation. Useful to assure the command was really accepted. In a case of an
error it throws a 'NXTException'.

Confirmation requires a change of the direction of NXT Bluetooth communication which takes around 30 ms. 
-}
lowspeedWriteConfirm :: InputPort -> RxDataLength -> TxData -> NXT ()
lowspeedWriteConfirm = lowspeedWrite' True

lowspeedWrite' :: Bool -> InputPort -> RxDataLength -> TxData -> NXT ()
lowspeedWrite' confirm input rx txdata
  | length txdata <= 16 && rx <= 16 = do
      when debug $ liftIO . hPutStrLn stderr $ "lowspeedwrite"
      let send = [request confirm, 0x0F, fromIntegral . fromEnum $ input] ++ (toUByte . length $ txdata) ++ toUByte rx ++ txdata
      sendData send
      when confirm $ do
        receive <- receiveData
        case receive of
          [0x02, 0x0F, 0x00] -> return ()
          [_, _, e]          -> liftIO $ failNXT "lowspeedWrite" e
          _                  -> liftIO $ failNXT' "lowspeedWrite"
  | otherwise                       = liftIO . throwIO $ PatternMatchFail "lowspeedWrite"

{-|
Reads data. The protocol does not support variable-length return packages so the response always contains 16 data bytes with invalid
data padded with zeros.
-}
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
    _:_:e:_                             -> liftIO $ failNXT "lowSpeedRead" e
    _                                   -> liftIO $ failNXT' "lowSpeedRead"

{-|
Gets the name of the currently running program, if any.
-}
getCurrentProgramName :: NXT (Maybe String)
getCurrentProgramName = do
  when debug $ liftIO . hPutStrLn stderr $ "getcurrentprogramname"
  let send = [0x00, 0x11]
  sendData send
  receive <- receiveData
  case receive of
    0x02:0x11:0x00:filename | length filename == 20 -> return $ Just $ dataToString0 filename
    0x02:0x11:0xEC:_                                -> return Nothing
    _:_:e:_                                         -> liftIO $ failNXT "getCurrentProgramName" e
    _                                               -> liftIO $ failNXT' "getCurrentProgramName"

{-|
Reads a message from the currently running program from a given remote inbox queue. A queue is limited to 5 messages.
It throws a 'NXTException' if there is no message in a remote inbox queue.
-}
messageRead :: RemoteInbox -> RemoveMessage -> NXT String
messageRead inbox remove = do
  m <- maybeMessageRead inbox remove
  case m of
    Just m' -> return m'
    Nothing -> liftIO $ failNXT "messageRead" 0x40

-- TODO: Could probably loop infinitely? Some timeout could be useful?
{-|
Same as 'messageWrite' but if there is no message in a given remote inbox queue it retries until there is.
-}
ensureMessageRead :: RemoteInbox -> RemoveMessage -> NXT String
ensureMessageRead inbox remove = do
  m <- maybeMessageRead inbox remove
  case m of
    Just m' -> return m'
    Nothing -> ensureMessageRead inbox remove

{-|
Same as 'messageWrite' but returns 'Nothing' if there is no message in a given remote inbox queue.
-}
maybeMessageRead :: RemoteInbox -> RemoveMessage -> NXT (Maybe String)
maybeMessageRead inbox remove = do
  when debug $ liftIO . hPutStrLn stderr $ "messageRead"
  let inbox' = fromIntegral . fromEnum $ inbox
      send = [0x00, 0x13, inbox', fromIntegral . fromEnum $ Inbox0, fromIntegral . fromEnum $ remove] -- local inbox number does not matter for PC, it is used only when master NXT reads from slave NXT
  sendData send
  receive <- receiveData
  case receive of
    0x02:0x13:0x00:inbox'':size:message
      | inbox'' == inbox' && length message == 59 && size <= 59 -> return $ Just $ dataToString0 message
    0x02:0x13:0x40:_                                            -> return Nothing
    _:_:e:_                                                     -> liftIO $ failNXT "messageRead" e
    _                                                           -> liftIO $ failNXT' "messageRead"

{-|
Helper function which stops all NXT brick activities: stops motors and disables sensors.
-}
stopEverything :: NXT ()
stopEverything = do
  when debug $ liftIO . hPutStrLn stderr $ "stopeverything"
  mapM_ stopMotor [A ..]
  mapM_ stopSensor [One ..]
    where stopMotor x = setOutputState x 0 [] RegulationModeIdle 0 MotorRunStateIdle 0
          stopSensor x = setInputMode x NoSensor RawMode

{-|
Shutdowns (powers off) the NXT brick. You have to manually turn it on again.
-}
shutdown :: NXT ()
shutdown = do
  when debug $ liftIO . hPutStrLn stderr $ "shutdown"
  mid <- getModuleID "IOCtrl.mod"
  writeIOMap (fromJust mid) 0 [0x00, 0x5A]

{-|
Opens a given file for writing as a linked list of flash sectors.
-}
openWrite :: FileName -> FileSize -> NXT FileHandle
openWrite filename filesize = do
  when debug $ liftIO . hPutStrLn stderr $ "openwrite"
  let send = [0x01, 0x81] ++ nameToData filename ++ toULong filesize
  sendData send
  receive <- receiveData
  case receive of
    [0x02, 0x81, 0x00, h] -> return $ fromUByte [h]
    _:_:e:_               -> liftIO $ failNXT "openWrite" e
    _                     -> liftIO $ failNXT' "openWrite"

{-|
Opens a given file for writing as a linear contiguous block of flash memory (required for user programs and certain data files).
-}
openWriteLinear :: FileName -> FileSize -> NXT FileHandle
openWriteLinear filename filesize = do
  when debug $ liftIO . hPutStrLn stderr $ "openwritelinear"
  let send = [0x01, 0x89] ++ nameToData filename ++ toULong filesize
  sendData send
  receive <- receiveData
  case receive of
    [0x02, 0x89, 0x00, h] -> return $ fromUByte [h]
    _:_:e:_               -> liftIO $ failNXT "openWriteLinear" e
    _                     -> liftIO $ failNXT' "openWriteLinear"

{-|
Writes data to a file. At most 61 bytes can be written at a time.
-}
write :: FileHandle -> FileData -> NXT ()
write = write' False

{-|
Same as 'write' but also request a confirmation. Useful to assure the command was really accepted. In a case of an error it
throws a 'NXTException'.

Confirmation requires a change of the direction of NXT Bluetooth communication which takes around 30 ms. 
-}
writeConfirm :: FileHandle -> FileData -> NXT ()
writeConfirm = write' True

write' :: Bool -> FileHandle -> FileData -> NXT ()
write' confirm filehandle filedata
  | length filedata <= 61 = do
      when debug $ liftIO . hPutStrLn stderr $ "write"
      let send = [request' confirm, 0x83] ++ toUByte filehandle ++ filedata
      sendData send
      when confirm $ do
        receive <- receiveData
        case receive of
          [0x02, 0x83, 0x00, h, bw1, bw2]
            | fromUByte [h] == filehandle && length filedata == fromUWord [bw1, bw2] -> return ()
          _:_:e:_                                                                    -> liftIO $ failNXT "write" e
          _                                                                          -> liftIO $ failNXT' "write"
  | otherwise             = liftIO . throwIO $ PatternMatchFail "write"

{-|
Closes a file.
-}
close :: FileHandle -> NXT ()
close = close' False

{-|
Same as 'close' but also request a confirmation. Useful to assure the command was really accepted. In a case of an error it
throws a 'NXTException'.

Confirmation requires a change of the direction of NXT Bluetooth communication which takes around 30 ms. 
-}
closeConfirm :: FileHandle -> NXT ()
closeConfirm = close' True

close' :: Bool -> FileHandle -> NXT ()
close' confirm filehandle = do
  when debug $ liftIO . hPutStrLn stderr $ "close"
  let send = [request' confirm, 0x84] ++ toUByte filehandle
  sendData send
  when confirm $ do
    receive <- receiveData
    case receive of
      [0x02, 0x84, 0x00, h]
        | fromUByte [h] == filehandle -> return ()
      _:_:e:_                         -> liftIO $ failNXT "close" e
      _                               -> liftIO $ failNXT' "close"

{-|
Deletes a given file.
-}
delete :: FileName -> NXT ()
delete = delete' False False

{-|
Same as 'delete' but also request a confirmation. Useful to assure the command was really accepted. In a case of an error it throws
a 'NXTException'.

Confirmation requires a change of the direction of NXT Bluetooth communication which takes around 30 ms. 
-}
deleteConfirm :: FileName -> NXT ()
deleteConfirm = delete' True False

{-|
Same as 'deleteConfirm' but it also requires that the file exists before deletion. It throws a 'NXTException' otherwise.
-}
deleteExisting :: FileName -> NXT ()
deleteExisting = delete' True True

delete' :: Bool -> Bool -> FileName -> NXT ()
delete' confirm existence filename = do
  when debug $ liftIO . hPutStrLn stderr $ "delete"
  let send = [request' confirm, 0x85] ++ nameToData filename
  sendData send
  when confirm $ do
    receive <- receiveData
    case receive of
      0x02:0x85:0x00:f
        | dataToString0 f == filename -> return ()
      0x02:0x85:0x87:_                -> when existence $ liftIO $ failNXT "delete" 0x87
      _:_:e:_                         -> liftIO $ failNXT "delete" e
      _                               -> liftIO $ failNXT' "delete"

-- TODO: Populate cache here?
{-|
Requests information about the first module matching a given module name (which can be a wild card). Returned module handle
can be used for followup requests and has to be closed when not needed anymore.
-}
requestFirstModule :: ModuleName -> NXT (ModuleHandle, Maybe ModuleInfo)
requestFirstModule modulename = do
  when debug $ liftIO . hPutStrLn stderr $ "requestfirstmodule"
  let send = [0x01, 0x90] ++ nameToData modulename
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
    _:_:e:_                 -> liftIO $ failNXT "requestFirstModule" e
    _                       -> liftIO $ failNXT' "requestFirstModule"

-- TODO: Populate cache here?
{-|
Requests information about the next module matching previously requested module name (which can be a wild card). Returned module
handle can be used for followup requests and has to be closed when not needed anymore.
-}
requestNextModule :: ModuleHandle -> NXT (ModuleHandle, Maybe ModuleInfo)
requestNextModule modulehandle = do
  when debug $ liftIO . hPutStrLn stderr $ "requestnextmodule"
  let send = [0x01, 0x91] ++ toUByte modulehandle
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
    _:_:e:_                 -> liftIO $ failNXT "requestNextModule" e
    _                       -> liftIO $ failNXT' "requestNextModule"

{-|
Closes module handle of previously requested module information.
-}
closeModuleHandle :: ModuleHandle -> NXT ()
closeModuleHandle = closeModuleHandle' False

{-|
Same as 'closeModuleHandle' but also request a confirmation. Useful to assure the command was really accepted. In a case of an
error it throws a 'NXTException'.

Confirmation requires a change of the direction of NXT Bluetooth communication which takes around 30 ms.
-}
closeModuleHandleConfirm :: ModuleHandle -> NXT ()
closeModuleHandleConfirm = closeModuleHandle' True

closeModuleHandle' :: Bool -> ModuleHandle -> NXT ()
closeModuleHandle' confirm modulehandle = do
  when debug $ liftIO . hPutStrLn stderr $ "closemodulehandle"
  let send = [request' confirm, 0x92] ++ toUByte modulehandle
  sendData send
  when confirm $ do
    receive <- receiveData
    case receive of
      [0x02, 0x92, 0x00, h]
        | fromUByte [h] == modulehandle -> return ()
      _:_:e:_                           -> liftIO $ failNXT "closeModuleHandle" e
      _                                 -> liftIO $ failNXT' "closeModuleHandle"

-- TODO: Populate cache here?
-- TODO: Use bracket to ensure closed module handle?
{-|
Helper function to get information about all modules matching a given module name (which can be a wild card).
-}
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
                (h'', mi'') <- next h'
                return (h'', mi':mi'')
              Nothing  -> return (h', [])

{-|
Reads data from an IO map of a given module. At most 119 bytes can be read at a time.

You probably have to know what different values at different positions mean and control. The best way is to check NXT firmware
source code.
-}
readIOMap :: ModuleID -> IOMapOffset -> IOMapLength -> NXT IOMapData
readIOMap moduleid offset len
  | offset >= 0 && len <= 119 = do
      when debug $ liftIO . hPutStrLn stderr $ "readiomap"
      let send = [0x01, 0x94] ++ toULong moduleid ++ toUWord offset ++ toUWord len
      sendData send
      receive <- receiveData
      case receive of
        0x02:0x94:0x00:mid1:mid2:mid3:mid4:r1:r2:values
          | fromULong [mid1, mid2, mid3, mid4] == moduleid && fromUWord [r1, r2] == len -> return values
        _:_:e:_                                                                         -> liftIO $ failNXT "readIOMap" e
        _                                                                               -> liftIO $ failNXT' "readIOMap"
  | otherwise                 = liftIO . throwIO $ PatternMatchFail "readIOMap"


{-|
Writes data to an IO map of a given module. At most 54 bytes can be written at a time.

You probably have to know what different values at different positions mean and control. The best way is to check NXT firmware
source code.
-}
writeIOMap :: ModuleID -> IOMapOffset -> IOMapData -> NXT ()
writeIOMap = writeIOMap' False

{-|
Same as 'writeIOMap' but also request a confirmation. Useful to assure the command was really accepted. In a case of an error
it throws a 'NXTException'.

Confirmation requires a change of the direction of NXT Bluetooth communication which takes around 30 ms. 
-}
writeIOMapConfirm :: ModuleID -> IOMapOffset -> IOMapData -> NXT ()
writeIOMapConfirm = writeIOMap' True

writeIOMap' :: Bool -> ModuleID -> IOMapOffset -> IOMapData -> NXT ()
writeIOMap' confirm moduleid offset mapdata
  | offset >= 0 && length mapdata <= 54 = do
      when debug $ liftIO . hPutStrLn stderr $ "writeiomap"
      let send = [request' confirm, 0x95] ++ toULong moduleid ++ toUWord offset ++ toUWord (length mapdata) ++ mapdata
      sendData send
      when confirm $ do
        receive <- receiveData
        case receive of
          [0x02, 0x95, 0x00, mid1, mid2, mid3, mid4, w1, w2]
            | fromULong [mid1, mid2, mid3, mid4] == moduleid && fromUWord [w1, w2] == length mapdata -> return ()
          _:_:e:_                                                                                    -> liftIO $ failNXT "writeIOMap" e
          _                                                                                          -> liftIO $ failNXT' "writeIOMap"
  | otherwise                           = liftIO . throwIO $ PatternMatchFail "writeIOMap"

{-|
Helper function to get an ID of a module matching a given module name. Each module encompass some firmware functionality.
Function caches IDs so it hopefully retrieves it from a cache of previous requests.
-}
getModuleID :: ModuleName -> NXT (Maybe ModuleID)
getModuleID modulename | '*' `elem` modulename = return Nothing -- we do not allow wild cards
                       | otherwise = do
                           mods <- getsNXT modules
                           let modulename' = map toLower modulename
                           case modulename' `lookup` mods of
                             Just (ModuleInfo _ mid _ _) -> return $ Just mid
                             Nothing                     -> do
                               (h, mi) <- requestFirstModule modulename'
                               closeModuleHandle h
                               case mi of
                                 Just mi'@(ModuleInfo _ mid _ _) -> do
                                   modifyNXT (\s -> s { modules = (modulename', mi'):mods })
                                   return $ Just mid
                                 Nothing                         -> return Nothing

request :: Bool -> Word8 
request confirm | confirm   = 0x00
                | otherwise = 0x80

request' :: Bool -> Word8 
request' confirm | confirm   = 0x01
                 | otherwise = 0x81
