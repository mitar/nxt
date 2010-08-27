module NXT.NXTUltrasonicSensor where

import Control.Exception
import Control.Monad
import Data.Word

import NXT.NXT
import NXT.NXTData
import NXT.NXTTypes

-- I2C communication with ultrasonics sensor is described in Lego Mindstorms NXT Hardware Developer Kit:
--  Appendix 7 - Ultrasonic sensor I2C communication protocol

-- Specification is vague whether zero, factor, divisor and measurement values are signed or unsigned

type DeviceAddress = Word8
type Command = Word8

type Zero = Int
type ScaleFactor = Int
type ScaleDivisor = Int
data CommandState =
    Off -- turns sensor off
  | SingleShot -- in this mode ultrasonic sensor only makes a new measurement every time the command byte is send to the sensor
               -- the sensor measures distances for up to 8 objects which can be retrieved with usReadMeasurement
  | ContinuousMeasurement -- this is the default mode, where the sensor continuously makes new measurement with the specific interval
  | EventCapture -- within this mode the sensor will measure whether any other ultrasonic sensors are within the vicinity
                 -- with this information a program can evaluate when it is best to make a new measurement which will not conflict with other ultrasonic sensors
  | WarmReset -- requests warm reset
  deriving (Bounded, Enum, Eq, Ord, Read, Show)
type ContinuousInterval = Int
type MeasurementNumber = Int
type Measurement = Maybe Int

deviceAddress :: DeviceAddress
deviceAddress = 0x02

usInit :: InputPort -> NXT ()
usInit input = do
  setInputModeConfirm input Lowspeed9V RawMode
  ready <- lowspeedGetStatus input
  when (ready > 0) $ do lowspeedRead input ; return () -- clears any possible pending data in the buffer
  usSetCommandState input WarmReset

usReadByte :: (Integral a) => InputPort -> Command -> NXT a
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

usGetVersion :: InputPort -> NXT String
usGetVersion input = usReadString input 0x00 8

usGetProductID :: InputPort -> NXT String
usGetProductID input = usReadString input 0x08 8

usGetSensorType :: InputPort -> NXT String
usGetSensorType input = usReadString input 0x10 8

usGetFactoryZero :: InputPort -> NXT Zero
usGetFactoryZero input = usReadByte input 0x11

usGetFactoryScaleFactor :: InputPort -> NXT ScaleFactor
usGetFactoryScaleFactor input = usReadByte input 0x12

usGetFactoryScaleDivisor :: InputPort -> NXT ScaleDivisor
usGetFactoryScaleDivisor input = usReadByte input 0x13

usGetMeasurementUnits :: InputPort -> NXT String
usGetMeasurementUnits input = usReadString input 0x14 7

-- Configuration

usGetCommandState :: InputPort -> NXT CommandState
usGetCommandState input = do
  state <- usReadByte input 0x41 :: NXT Int
  case state of
    0x00 -> return Off
    0x01 -> return SingleShot
    0x02 -> return ContinuousMeasurement
    0x03 -> return EventCapture
    0x04 -> return WarmReset
    _    -> throw $ PatternMatchFail "usGetCommandState"

usGetContinuousInterval :: InputPort -> NXT ContinuousInterval
usGetContinuousInterval input = usReadByte input 0x40

usGetActualZero :: InputPort -> NXT Zero
usGetActualZero input = usReadByte input 0x50

usGetActualScaleFactor :: InputPort -> NXT ScaleFactor
usGetActualScaleFactor input = usReadByte input 0x51

usGetActualScaleDivisor :: InputPort -> NXT ScaleDivisor
usGetActualScaleDivisor input = usReadByte input 0x52

usSetCommandState :: InputPort -> CommandState -> NXT ()
usSetCommandState input state = lowspeedWriteConfirm input 0 [deviceAddress, 0x41, fromIntegral . fromEnum $ state]

usSetContinuousInterval :: InputPort -> ContinuousInterval -> NXT ()
usSetContinuousInterval input interval = lowspeedWrite input 0 $ [deviceAddress, 0x40] ++ (toUByte interval)

usSetActualZero :: InputPort -> Zero -> NXT ()
usSetActualZero input zero = lowspeedWrite input 0 $ [deviceAddress, 0x50] ++ (toUByte zero)

usSetActualScaleFactor :: InputPort -> ScaleFactor -> NXT ()
usSetActualScaleFactor input factor = lowspeedWrite input 0 $ [deviceAddress, 0x51] ++ (toUByte factor)

usSetActualScaleDivisor :: InputPort -> ScaleDivisor -> NXT ()
usSetActualScaleDivisor input divisor = lowspeedWrite input 0 $ [deviceAddress, 0x52] ++ (toUByte divisor)

-- Measurement

usGetMeasurement :: InputPort -> MeasurementNumber -> NXT Measurement
usGetMeasurement input number | number >= 0 && number < 8 = do measurement <- usReadByte input $ 0x42 + (fromIntegral number)
                                                               if measurement == 0xFF
                                                                 then return Nothing
                                                                 else return $ Just measurement
                              | otherwise                 = throw . PatternMatchFail $ "usGetMeasurement"
