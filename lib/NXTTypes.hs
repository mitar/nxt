module NXT.NXTTypes where

import Control.Monad.State
import Data.Int
import Data.Ratio
import Data.Time.Clock.POSIX
import Data.Word
import System.IO

-- Described in Lego Mindstorms NXT Bluetooth Developer Kit:
--  Appendix 1 - Communication protocol
--  Appendix 2 - Direct commands

-- I2C communication with ultrasonics sensor is described in Lego Mindstorms NXT Hardware Developer Kit:
--  Appendix 7 - Ultrasonic sensor I2C communication protocol

type NXT = StateT NXTState IO -- NXT monad
data NXTState = NXTState { nxthandle :: Handle, address :: (Maybe BTAddress), modules :: [(ModuleName, ModuleInfo)], sleeptime :: Duration, lastkeepalive :: POSIXTime } -- NXT monad has a handle of an opened serial port, some module infos, sleep time limit in seconds, last time keep alive has been sent

-- The format of version is major.minor: (printf "%d.%02d" major minor)
type Major = Int
type Minor = Int
data FirmwareVersion = FirmwareVersion Major Minor deriving (Eq, Show)
data ProtocolVersion = ProtocolVersion Major Minor deriving (Eq, Show)
data Version = Version FirmwareVersion ProtocolVersion deriving (Eq, Show)

type Name = String
type BTAddress = String -- address in the string format
type BTStrength = Int64 -- it seems it is not reported, use bluetoothRSSI or bluetoothLinkQuality
type FlashFree = Int64 -- in bytes
data DeviceInfo = DeviceInfo Name BTAddress BTStrength FlashFree deriving (Eq, Show)

type Frequency = Int -- in Hz
type Duration = Ratio Integer -- in s
type Voltage = Int -- in mV

data OutputPort = A | B | C deriving (Bounded, Enum, Eq, Ord, Read, Show)
data InputPort = One | Two | Three | Four deriving (Bounded, Enum, Eq, Ord, Read, Show)

type OutputPower = Int -- power and direction, in [-100, 100] range
data OutputMode =
    MotorOn -- enables PWM power according to speed
  | Brake -- voltage is not allowed to float between PWM pulses, improves accuracy, uses more power
  | Regulated -- required in conjunction with output regulation mode setting
  deriving (Eq, Show)
data RegulationMode =
    RegulationModeIdle -- disables regulation
  | RegulationModeMotorSpeed -- auto adjust PWM duty cycle if motor is affected by physical load
  | RegulationModeMotorSync -- attempts to keep rotation in sync with another motor that has this set, also involves turn ratio
  deriving (Eq, Show)
type TurnRatio = Int -- in regulated synced mode the difference between two motors, in [-100, 100] range
data RunState =
    MotorRunStateIdle -- disables power to motor
  | MotorRunStateRampUp -- ramping to a new speed set-point that is greater than the current speed set-point
  | MotorRunStateRunning -- enables power to motor
  | MotorRunStateRampDown -- ramping to a new speed set-point that is less than the current speed set-point
  deriving (Eq, Show)
type TachoLimit = Int64 -- unsigned long
type TachoCount = Int64 -- signed long
type BlockTachoCount = Int64 -- signed long
type RotationCount = Int64 -- signed long
data OutputState = OutputState OutputPort OutputPower [OutputMode] RegulationMode TurnRatio RunState TachoLimit TachoCount BlockTachoCount RotationCount deriving (Eq, Show)

data Valid = Valid | NotValid deriving (Eq, Show)
data Calibrated = Calibrated | NotCalibrated deriving (Eq, Show)
data SensorType = NoSensor | Switch | Temperature | Reflection | Angle | LightActive | LightInactive | SoundDB | SoundDBA | Custom | Lowspeed | Lowspeed9V | NoOfSensorTypes deriving (Eq, Show)
data SensorMode =
    RawMode -- reports scaled value equal to the raw value
  | BooleanMode -- reports scaled value as 1 true or 0 false, false if raw value > 55% of total range, true if < 45%
  | TransitionCntMode -- reports scaled value as number of transitions between true and false
  | PeriodCounterMode -- reports scaled value as number of transitions from false to true, then back to false
  | PctFullScaleMode -- reports scaled value as % of full scale reading for configured sensor type
  | CelsiusMode
  | FahrenheitMode
  | AngleStepsMode -- reports scaled value as count of ticks on RCX-style rotation sensor
  -- two modes unnecessary?
  -- | SlopeMask
  -- | ModeMask
  deriving (Eq, Show)
type RawADValue = Int
type NormalizedADValue = Int -- normalized A/D value, in [0, 1023] range
type ScaledValue = Int -- in %
type CalibratedValue = Int -- currently unused
data InputValues = InputValues InputPort Valid Calibrated SensorType SensorMode RawADValue NormalizedADValue ScaledValue CalibratedValue deriving (Eq, Show)

data MotorReset =
    AbsolutePosition -- resets program-relative position counter (rotation count)
  | RelativePosition -- resets block-relative position counter (block tacho count)
  | InternalPosition -- resets internal movement counters, cancels current goal and resets internal error-correction system
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

type FileName = String -- 15.3 format
type FileSize = Int -- unsigned long
type FileHandle = Int -- unsigned byte
type FileData = [Word8]
type RxDataLength = Int
type TxData = [Word8]
type RxData = [Word8]

type ModuleHandle = Int -- unsigned byte
type ModuleName = String -- module name extension is .mod
type ModuleID = Int64 -- unsigned long
type ModuleSize = Int64 -- unsigned long
type ModuleIOMapSize = Int -- unsigned word
data ModuleInfo = ModuleInfo ModuleName ModuleID ModuleSize ModuleIOMapSize deriving (Eq, Show)

type IOMapOffset = Int -- unsigned word
type IOMapLength = Int -- unsigned word
type IOMapData = [Word8]
