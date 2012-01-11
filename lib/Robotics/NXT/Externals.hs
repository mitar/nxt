{-# LANGUAGE DeriveDataTypeable #-}

module Robotics.NXT.Externals where

import Control.Exception
import Data.Int
import Data.Ratio
import Data.Time.Clock
import Data.Typeable
import Data.Word

-- Described in Lego Mindstorms NXT Bluetooth Developer Kit:
--  Appendix 1 - Communication protocol
--  Appendix 2 - Direct commands

-- I2C communication with ultrasonics sensor is described in Lego Mindstorms NXT Hardware Developer Kit:
--  Appendix 7 - Ultrasonic sensor I2C communication protocol

-- | The format of version is \"major.minor\". To format it use @'printf' \"%d.%02d\" major minor@.
data Version = Version FirmwareVersion ProtocolVersion deriving (Bounded, Eq, Ord, Read, Show)
data FirmwareVersion = FirmwareVersion Major Minor deriving (Bounded, Eq, Ord, Read, Show)
data ProtocolVersion = ProtocolVersion Major Minor deriving (Bounded, Eq, Ord, Read, Show)
type Major = Int
type Minor = Int

data DeviceInfo = DeviceInfo Name BTAddress BTStrength FlashFree deriving (Eq, Ord, Read, Show)
-- | Name of the NXT brick.
type Name = String
-- | Bluetooth address of the NXT brick in the string format.
type BTAddress = String
-- | Strength of the Bluetooth signal. Not implemented in current NXT firmware versions. Use 'bluetoothRSSI' or 'bluetoothLinkQuality' as an alternative.
type BTStrength = Int64
-- | Free flash space on the NXT brick (in bytes).
type FlashFree = Int64

data OutputState = OutputState OutputPort OutputPower [OutputMode] RegulationMode TurnRatio RunState TachoLimit TachoCount BlockTachoCount RotationCount deriving (Eq, Ord, Read, Show)

data OutputPort =
    A -- ^ Output port (motor) A.
  | B -- ^ Output port (motor) B.
  | C -- ^ Output port (motor) C.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)
-- | Power and direction. In [-100, 100] range.
type OutputPower = Int
data OutputMode =
    MotorOn -- ^ Enables PWM power according to speed.
  | Brake -- ^ Voltage is not allowed to float between PWM pulses, improves accuracy, uses more power.
  | Regulated -- ^ Required in conjunction with output regulation mode setting.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)
data RegulationMode =
    RegulationModeIdle -- ^ Disables regulation.
  | RegulationModeMotorSpeed -- ^ Auto adjust PWM duty cycle if motor is affected by physical load. Really works only if there is
                             -- room for that (not that motor is already running at the maximum power).
  | RegulationModeMotorSync -- ^ Attempts to keep rotation in sync with another motor that has this set. Also involves turn ratio.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)
-- | In regulated synced mode the difference between two motors. In [-100, 100] range.
type TurnRatio = Int
data RunState =
    MotorRunStateIdle -- ^ Disables power to motor.
  | MotorRunStateRampUp -- ^ Ramping to a new speed set-point that is greater than the current speed set-point.
  | MotorRunStateRunning -- ^ Enables power to motor.
  | MotorRunStateRampDown -- ^ Ramping to a new speed set-point that is less than the current speed set-point.
  | MotorRunStateHold -- ^ Hold at the current position.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Target tacho limit for a motor movement. 0 means no limit. It is an unsigned value (you select direction of motor movement with
-- sign of 'OutputPower' value).
type TachoLimit = Int64 -- unsigned long
-- | Internal (absolute) tacho counter. Number since the last reset of the motor tacho counter.
type TachoCount = Int64 -- signed long
-- | Block-relative position counter. Current position relative to the last programmed movement.
type BlockTachoCount = Int64 -- signed long
-- | Program-relative position counter. Current position relative to the last reset of the rotation sensor for this motor.
type RotationCount = Int64 -- signed long

data MotorReset =
    AbsolutePosition -- ^ Resets program-relative position counter ('RotationCount').
  | RelativePosition -- ^ Resets block-relative position counter ('BlockTachoCount')
  | InternalPosition -- ^ Resets internal movement counters (also 'TachoCount'), cancels current goal and resets internal error-correction system.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

data InputPort =
    One -- ^ Input port (sensor) 1.
  | Two -- ^ Input port (sensor) 2.
  | Three -- ^ Input port (sensor) 3.
  | Four -- ^ Input port (sensor) 4.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

data InputValue = InputValue InputPort Valid Calibrated SensorType SensorMode RawADValue NormalizedADValue ScaledValue CalibratedValue deriving (Bounded, Eq, Ord, Read, Show)

-- | 'True' if new data value should be seen as valid data.
type Valid = Bool
-- | 'True' if calibration file found and used for 'CalibratedValue'.
type Calibrated = Bool
-- | Type of the sensor currently attached to 'InputPort'. 'NoSensor' turns off the sensor.
data SensorType = NoSensor | Switch | Temperature | Reflection | Angle | LightActive | LightInactive | SoundDB | SoundDBA | Custom | Lowspeed | Lowspeed9V | NoOfSensorTypes deriving (Bounded, Enum, Eq, Ord, Read, Show)
data SensorMode =
    RawMode -- ^ Reports scaled value equal to the raw value.
  | BooleanMode -- ^ Reports scaled value as 1 true or 0 false, false if raw value > 55% of total range, true if < 45%.
  | TransitionCntMode -- ^ Reports scaled value as number of transitions between true and false.
  | PeriodCounterMode -- ^ Reports scaled value as number of transitions from false to true, then back to false.
  | PctFullScaleMode -- ^ Reports scaled value as % of full scale reading for a configured sensor type.
  | CelsiusMode -- ^ For reporting temperature in celsius.
  | FahrenheitMode -- ^ For reporting temperature in fahrenheit.
  | AngleStepsMode -- ^ Reports scaled value as count of ticks on RCX-style rotation sensor.
  
  -- two modes unnecessary?
  -- SlopeMask
  -- ModeMask
  deriving (Bounded, Enum, Eq, Ord, Read, Show)
-- | Raw A/D value. Device dependent.
type RawADValue = Int
-- | Normalized A/D value. Type dependent. In [0, 1023] range.
type NormalizedADValue = Int
-- | Scaled value. Mode dependent. In percent.
type ScaledValue = Int
-- | Value scaled according to calibration. Unused in current NXT firmware versions.
type CalibratedValue = Int

-- | Voltage value (in volts).
type Voltage = Ratio Int
-- | Time duration (in seconds).
type Duration = NominalDiffTime

-- | Inbox on the NXT brick into which the host (computer) queues messages for the program running there.
data Inbox = Inbox0 | Inbox1 | Inbox2 | Inbox3 | Inbox4 | Inbox5 | Inbox6 | Inbox7 | Inbox8 | Inbox9 deriving (Bounded, Enum, Eq, Ord, Read, Show)
-- | Outbox on the NXT brick where the program running there queues messages for the host (computer).
-- There is a convention that only 'RemoteInbox10' - 'RemoteInbox19' outboxes are used for this purpose so that lower ones can
-- be used for inter-brick communication. But this convention is not really obeyed in practice.
data RemoteInbox = RemoteInbox0 | RemoteInbox1 | RemoteInbox2 | RemoteInbox3 | RemoteInbox4 | RemoteInbox5 | RemoteInbox6 | RemoteInbox7 | RemoteInbox8 | RemoteInbox9 | RemoteInbox10 | RemoteInbox11 | RemoteInbox12 | RemoteInbox13 | RemoteInbox14 | RemoteInbox15 | RemoteInbox16 | RemoteInbox17 | RemoteInbox18 | RemoteInbox19 deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Should the message be remove from the queue once received?
type RemoveMessage = Bool

-- | Loop playback of the sound file?
type LoopPlayback = Bool
-- | Frequency of the played tone (in hertz).
type Frequency = Int

-- | At most 16 data bytes can be read at a time.
type RxDataLength = Int

type TxData = [Word8]
type RxData = [Word8]

-- | Address of the device (sensor) on the I2C bus.
type DeviceAddress = Word8
-- | I2C device command.
type Command = Word8

-- | I2C device measurement value.
type Measurement = Int -- specification is vague whether measurement value is signed or unsigned

-- | Filename of the file on the NXT brick filesystem. In 15.3 format.
type FileName = String
-- | Size of the file on the NXT brick filesystem.
type FileSize = Int -- unsigned long
-- | Handle of the opened file on the NXT brick filesystem.
type FileHandle = Int -- unsigned byte

type FileData = [Word8]

-- | Type of the IO map module information.
data ModuleInfo = ModuleInfo ModuleName ModuleID ModuleSize ModuleIOMapSize deriving (Eq, Ord, Read, Show)
-- | Module name extension is @.mod@. For some functions this can be also a wild card.
type ModuleName = String
type ModuleID = Int64 -- unsigned long
type ModuleSize = Int64 -- unsigned long
type ModuleIOMapSize = Int -- unsigned word

-- | Handle for traversing of modules. Only one module handle can be opened at a time so be careful to close them when not
-- needed anymore.
type ModuleHandle = Int -- unsigned byte

type IOMapOffset = Int -- unsigned word
type IOMapLength = Int -- unsigned word
type IOMapData = [Word8]

-- | Timeout exception for NXT IO operations.
data TimeoutException = TimoutException deriving (Show, Typeable)
instance Exception TimeoutException
