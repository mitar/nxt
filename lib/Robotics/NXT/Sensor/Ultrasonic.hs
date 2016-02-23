{-|
This module defines an interface to a digital ultrasonic sensor of the NXT kit.

I2C communication with ultrasonics sensor is described in Lego Mindstorms NXT Hardware Developer Kit,
Appendix 7 - Ultrasonic sensor I2C communication protocol.
-}

module Robotics.NXT.Sensor.Ultrasonic (
  -- * Initialization
  usInit,
  -- * Measurement
  usGetMeasurement,
  usGetAllMeasurements,
  -- * Configuration
  usSetMode,
  usGetMode,
  Mode(..),
  usSetContinuousInterval,
  usGetContinuousInterval,
  usSetActualZero,
  usGetActualZero,
  usSetActualScaleFactor,
  usGetActualScaleFactor,
  usSetActualScaleDivisor,
  usGetActualScaleDivisor,
  -- * Constants
  usGetVersion,
  usGetVendorID,
  usGetDeviceID,
  usGetFactoryZero,
  usGetFactoryScaleFactor,
  usGetFactoryScaleDivisor,
  usGetMeasurementUnits,
  -- * Types
  Zero,
  ScaleFactor,
  ScaleDivisor,
  ContinuousInterval,
  MeasurementNumber
) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.Maybe

import Robotics.NXT
import Robotics.NXT.Data

-- Specification is vague whether zero, factor and divisor values are signed or unsigned

-- | Type of a zero value.
type Zero = Int
-- | Type of a scale factor value.
type ScaleFactor = Int
-- | Type of a scale divisor value.
type ScaleDivisor = Int
{-|
Type of a continuous measurement interval value. This seems to be in the range 1-15.
-}
type ContinuousInterval = Int
{-|
Type of a measurement number. Sensor stores measurements (distances) for the first 8 echoes (numbered 0-7) it receives
in 'SingleShot' mode. For 'ContinuousMeasurement' and 'EventCapture' modes use first (0) measurement (distance) number.
-}
type MeasurementNumber = Int

data Mode =
    Off -- ^ Turns sensor off.
  | SingleShot -- ^ In this mode ultrasonic sensor only makes a new measurement every time this mode is set.
               -- The sensor measures distances for up to 8 objects (8 first echoes) which can be retrieved with 'usGetMeasurement'.
  | ContinuousMeasurement -- ^ In this mode the sensor continuously makes new measurement with the specific interval. This is the default mode. 
  | EventCapture -- ^ Within this mode the sensor will measure whether any other ultrasonic sensors are within the vicinity.
                 -- With this information a program can evaluate when it is best to make a new measurement which will not conflict with other ultrasonic sensors.
  | WarmReset -- ^ Requests warm reset.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

deviceAddress :: DeviceAddress
deviceAddress = 0x02

{-|
Initializes sensor on the given input port. Default is 'ContinuousMeasurement' mode.
-}
usInit :: InputPort -> NXT ()
usInit input = do
  setInputModeConfirm input Lowspeed9V RawMode
  ready <- lowspeedGetStatus input
  when (ready > 0) $ lowspeedRead input >> return () -- clears any possible pending data in the buffer
  usSetMode input WarmReset

usReadByte :: Integral a => InputPort -> Command -> NXT a
usReadByte input command = do
  lowspeedWrite input 1 [deviceAddress, command]
  b <- lowspeedRead input
  return $ fromUByte . take 1 $ b

usReadString :: InputPort -> Command -> RxDataLength -> NXT String
usReadString input command size = do
  lowspeedWrite input size [deviceAddress, command]
  s <- lowspeedRead input
  return $ dataToString0 s

-- Constants

{-|
Reads software version string (@V1.0@).
-}
usGetVersion :: InputPort -> NXT String
usGetVersion input = usReadString input 0x00 8

{-|
Reads vendor ID string (@LEGO@).
-}
usGetVendorID :: InputPort -> NXT String
usGetVendorID input = usReadString input 0x08 8

{-|
Reads device ID string (@Sonar@).
-}
usGetDeviceID :: InputPort -> NXT String
usGetDeviceID input = usReadString input 0x10 8

{-|
Reads factory zero value.
-}
usGetFactoryZero :: InputPort -> NXT Zero
usGetFactoryZero input = usReadByte input 0x11

{-|
Reads factory scale factor value.
-}
usGetFactoryScaleFactor :: InputPort -> NXT ScaleFactor
usGetFactoryScaleFactor input = usReadByte input 0x12

{-|
Reads factory scale divisor value.
-}
usGetFactoryScaleDivisor :: InputPort -> NXT ScaleDivisor
usGetFactoryScaleDivisor input = usReadByte input 0x13

{-|
Reads measurement units string (@10E-2m@, a centimeter).
-}
usGetMeasurementUnits :: InputPort -> NXT String
usGetMeasurementUnits input = usReadString input 0x14 7

-- Configuration

{-|
Gets current mode of operation.
-}
usGetMode :: InputPort -> NXT Mode
usGetMode input = do
  mode <- usReadByte input 0x41 :: NXT Int
  case mode of
    0x00 -> return Off
    0x01 -> return SingleShot
    0x02 -> return ContinuousMeasurement
    0x03 -> return EventCapture
    0x04 -> return WarmReset
    _    -> liftIO . throwIO $ PatternMatchFail "usGetMode"

{-|
Gets current continuous measurement interval value.
-}
usGetContinuousInterval :: InputPort -> NXT ContinuousInterval
usGetContinuousInterval input = usReadByte input 0x40

{-|
Gets current (actual) zero value.
-}
usGetActualZero :: InputPort -> NXT Zero
usGetActualZero input = usReadByte input 0x50

{-|
Gets current (actual) scale factor value.
-}
usGetActualScaleFactor :: InputPort -> NXT ScaleFactor
usGetActualScaleFactor input = usReadByte input 0x51

{-|
Gets current (actual) scale divisor value.
-}
usGetActualScaleDivisor :: InputPort -> NXT ScaleDivisor
usGetActualScaleDivisor input = usReadByte input 0x52

{-|
Sets current mode of operation.
-}
usSetMode :: InputPort -> Mode -> NXT ()
usSetMode input mode = lowspeedWriteConfirm input 0 [deviceAddress, 0x41, fromIntegral . fromEnum $ mode]

{-|
Sets current continuous measurement interval value.
-}
usSetContinuousInterval :: InputPort -> ContinuousInterval -> NXT ()
usSetContinuousInterval input interval = lowspeedWrite input 0 $ [deviceAddress, 0x40] ++ toUByte interval

{-|
Sets current (actual) zero value. This is used to calibrate sensor.
-}
usSetActualZero :: InputPort -> Zero -> NXT ()
usSetActualZero input zero = lowspeedWrite input 0 $ [deviceAddress, 0x50] ++ toUByte zero

{-|
Sets current (actual) scale factor value. This is used to calibrate sensor.
-}
usSetActualScaleFactor :: InputPort -> ScaleFactor -> NXT ()
usSetActualScaleFactor input factor = lowspeedWrite input 0 $ [deviceAddress, 0x51] ++ toUByte factor

{-|
Sets current (actual) scale divisor value. This is used to calibrate sensor.
-}
usSetActualScaleDivisor :: InputPort -> ScaleDivisor -> NXT ()
usSetActualScaleDivisor input divisor = lowspeedWrite input 0 $ [deviceAddress, 0x52] ++ toUByte divisor

-- Measurement

{-|
Gets last measurement for a given measurement number based on the current mode. To retrieve new 'SingleShot' measurements first
use 'usSetMode' (with 'SingleShot' as an argument) to send new ultrasonic ping and after approximately 20ms read the results. (Change
of NXT Bluetooth communication direction takes around 30 ms.) In 'ContinuousMeasurement' mode new measurements are made automatically
based on the continuous measurement interval value.
-}
usGetMeasurement :: InputPort -> MeasurementNumber -> NXT (Maybe Measurement)
usGetMeasurement input number | number >= 0 && number < 8 = do measurement <- usReadByte input $ 0x42 + fromIntegral number
                                                               if measurement == 0xFF
                                                                 then return Nothing
                                                                 else return $ Just measurement
                              | otherwise                 = liftIO . throwIO $ PatternMatchFail "usGetMeasurement"

{-|
Helper function which gets all measurements available in order (closer first).
-}
usGetAllMeasurements :: InputPort -> NXT [Measurement]
usGetAllMeasurements input = mapMaybeM (usGetMeasurement input) [0..7]
  where mapMaybeM f as = catMaybes <$> mapM f as
