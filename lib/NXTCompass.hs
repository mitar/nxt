module NXT.NXTCompass where

import Control.Monad
import Data.Word

import NXT.NXT
import NXT.NXTData
import NXT.NXTTypes

-- Described in CMPS-Nx-V20-User-Guide.pdf at www.mindsensors.com

type DeviceAddress = Word8
type Command = Word8

data Mode =
    AutoTrigOn -- continuos measuring?
  | AutoTrigOff
  | ResultByte -- result is a byte mapped to [0-255)
  | ResultInteger -- result is an integer mapped to [0-3600)
  | Frequency50 -- sampling frequency 50 Hz (Europe standard)
  | Frequency60 -- sampling frequency 60 Hz (USA standard)
  | ADPAOn -- Auto Detecting Parallel Architecture on (several sensors on the same port)
  | ADPAOff -- Auto Detecting Parallel Architecture on
  | BeginCalibration
  | EndCalibration
  deriving Show

type Measurement = Int

deviceAddress :: DeviceAddress
deviceAddress = 0x02

csInit :: InputPort -> NXT ()
csInit input = do
  setInputModeConfirm input Lowspeed9V RawMode
  ready <- lowspeedGetStatus input
  when (ready > 0) $ do lowspeedRead input ; return () -- clears any possible pending data in the buffer
  csSetMode input ResultInteger

csReadByte :: (Integral a) => InputPort -> Command -> NXT a
csReadByte input command = do
  lowspeedWrite input 1 [deviceAddress, command]
  b <- lowspeedRead input
  return $ fromUByte . take 1 $ b

csReadString :: InputPort -> Command -> RxDataLength -> NXT String
csReadString input command size = do
  lowspeedWrite input size [deviceAddress, command]
  s <- lowspeedRead input
  return $ dataToString0 s

-- Constants

csGetVersion :: InputPort -> NXT String
csGetVersion input = csReadString input 0x00 8

csGetProductID :: InputPort -> NXT String
csGetProductID input = csReadString input 0x08 8

csGetSensorType :: InputPort -> NXT String
csGetSensorType input = csReadString input 0x10 8

-- Configuration

csSetMode :: InputPort -> Mode -> NXT ()
csSetMode input mode = do
  lowspeedWriteConfirm input 0 $ [deviceAddress, 0x41] ++ command
    where command = case mode of
                      AutoTrigOn       -> [0x41, 0x02]
                      AutoTrigOff      -> [0x53, 0x01]
                      ResultByte       -> [0x42]
                      ResultInteger    -> [0x49]
                      Frequency50      -> [0x45]
                      Frequency60      -> [0x55]
                      ADPAOn           -> [0x4E]
                      ADPAOff          -> [0x4F]
                      BeginCalibration -> [0x43]
                      EndCalibration   -> [0x44]

-- Measurement

csGetMeasurement :: InputPort -> NXT Measurement
csGetMeasurement input = do
  lowspeedWrite input 2 [deviceAddress, 0x42]
  angle <- lowspeedRead input
  return $ fromUWord angle
