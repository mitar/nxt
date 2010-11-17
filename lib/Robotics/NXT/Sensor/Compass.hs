{-|
This module defines an interface to @CMPS-Nx@ digital compass sensor from <http://www.mindsensors.com/> according to
@CMPS-Nx-V20-User-Guide.pdf@ documentation.
-}

module Robotics.NXT.Sensor.Compass (
  -- * Initialization
  csInit,
  -- * Measurement
  csGetMeasurement,
  -- * Configuration
  csSetMode,
  Mode(..),
  -- * Constants
  csGetVersion,
  csGetVendorID,
  csGetDeviceID
) where

import Control.Monad

import Robotics.NXT
import Robotics.NXT.Data

data Mode =
    AutoTrigOn -- ^ AutoTrig (continuous measuring) on.
  | AutoTrigOff -- ^ AutoTrig (continuous measuring) off.
  | ResultByte -- ^ Result is a byte mapped to [0-255).
  | ResultInteger -- ^ Result is an integer mapped to [0-3600).
  | Frequency50 -- ^ Sampling frequency 50 Hz (Europe standard).
  | Frequency60 -- ^ Sampling frequency 60 Hz (USA standard).
{-
ADPA (Auto Detecting Parallel Architecture) mode allows multiple sensors to be connected on the same bus (port). This mode is not
available for default address of 0x02 which is used by this module.
  | ADPAOn -- ^ Auto Detecting Parallel Architecture on.
  | ADPAOff -- ^ Auto Detecting Parallel Architecture off.
-}
  | BeginCalibration -- ^ Begin calibration mode.
  | EndCalibration -- ^ End calibration mode.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

deviceAddress :: DeviceAddress
deviceAddress = 0x02

{-|
Initializes sensor on the given input port. It sets 'Mode' to 'ResultInteger'.
-}
csInit :: InputPort -> NXT ()
csInit input = do
  setInputModeConfirm input Lowspeed9V RawMode
  ready <- lowspeedGetStatus input
  when (ready > 0) $ lowspeedRead input >> return () -- clears any possible pending data in the buffer
  csSetMode input ResultInteger

csReadString :: InputPort -> Command -> RxDataLength -> NXT String
csReadString input command size = do
  lowspeedWrite input size [deviceAddress, command]
  s <- lowspeedRead input
  return $ dataToString0 s

-- Constants

{-|
Reads software version string (@V2.00@).
-}
csGetVersion :: InputPort -> NXT String
csGetVersion input = csReadString input 0x00 8

{-|
Reads vendor ID string (@mndsnsrs@).
-}
csGetVendorID :: InputPort -> NXT String
csGetVendorID input = csReadString input 0x08 8

{-|
Reads device ID string (@CMPS@).
-}
csGetDeviceID :: InputPort -> NXT String
csGetDeviceID input = csReadString input 0x10 8

-- Configuration

{-|
Sets current mode of operation.
-}
csSetMode :: InputPort -> Mode -> NXT ()
csSetMode input mode =
  lowspeedWriteConfirm input 0 $ [deviceAddress, 0x41] ++ command
    where command = case mode of
                      AutoTrigOn       -> [0x41, 0x02]
                      AutoTrigOff      -> [0x53, 0x01]
                      ResultByte       -> [0x42]
                      ResultInteger    -> [0x49]
                      Frequency50      -> [0x45]
                      Frequency60      -> [0x55]
{-
Currently not used.
                      ADPAOn           -> [0x4E]
                      ADPAOff          -> [0x4F]
-}
                      BeginCalibration -> [0x43]
                      EndCalibration   -> [0x44]

-- Measurement

{-|
Gets last measurement. Based on current 'Mode' it return value in [0-255) ('ResultByte' mode) or [0-3600) ('ResultInteger' mode) range.
-}
csGetMeasurement :: InputPort -> NXT Measurement
csGetMeasurement input = do
  lowspeedWrite input 2 [deviceAddress, 0x42]
  angle <- lowspeedRead input
  return $ fromUWord angle
