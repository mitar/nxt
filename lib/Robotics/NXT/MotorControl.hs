{-|
A message-based control of a NXT brick via remotely executed @MotorControl@ program. The main goal of this approach is to achieve
better precision of motor movements as program monitors and adapts motor commands on the NXT brick itself. For example, common
motor's overshoot is thus removed in most cases.

You should download\/compile @MotorControl@ program from <http://www.mindstorms.rwth-aachen.de/trac/wiki/MotorControl> and upload
it to the NXT brick with @nxt-upload@ program. This module will then run that program and sends it messages to control the NXT brick.
Because commands will be executed and controlled directly on the NXT brick more powerful and precise control is possible.

Please refer to original documentation of @MotorControl@ program for more information, description of commands and explanations
how to use them. This interface also implements static pauses between commands mentioned there and required by @MotorControl@. This
means functions calls will block for this time so consider using special Haskell thread for communication with the NXT brick.
Dynamic\/minimal pauses are not implemented and have to be taken care of by user of this interface.

Check "Robotics.NXT.Remote" for another (simpler) approach to message-based control. It would be great to one day combine
@MotorControl@'s precision with API (especially interruptability) of @remote.nxc@.
-}

module Robotics.NXT.MotorControl (
  -- * Initialization
  startMotorControlProgram,
  stopMotorControlProgram,
  -- * Control
  controlledMotorCmd,
  MotorControlMode(..),
  resetErrorCorrection,
  isMotorReady,
  classicMotorCmd,
  -- * Types
  SpeedRegulation
) where

import Control.Concurrent
import Control.Exception
import Control.Monad.State
import Data.List
import Data.Maybe
import Text.Printf

import Robotics.NXT

-- TODO: Implement dynamic/minimal pauses internally.

programFilename :: FileName
programFilename = "MotorControl22.rxe"

{-|
Interface to @CONTROLLED_MOTORCMD@ command which takes care of precise motor movements.

Requires dynamic\/minimal pauses.
-}
controlledMotorCmd :: [OutputPort] -> OutputPower -> TachoLimit -> [MotorControlMode] -> NXT ()
controlledMotorCmd ports power limit modes = motorControlSend message
  where message = "1" ++ fromPorts ports ++ fromPower power ++ fromLimit limit ++ modes'
        modes' = show . sum . zipWith (*) [1, 2, 4] . map (fromEnum . flip elem modes) $ [HoldBrake ..]

{-|
Interface to @RESET_ERROR_CORRECTION@ command which can be used to reset the NXT brick's internal error correction mechanism (and
motor position information at the same time).

The same thing can be achieved by using 'resetMotorPosition' with 'InternalPosition' argument.
-}
resetErrorCorrection :: [OutputPort] -> NXT ()
resetErrorCorrection ports = motorControlSend message
  where message = '2' : ports'
        ports' = fromPorts ports

{-|
Interface to @ISMOTORREADY@ command which determine the state of a motor: is it currently executing a command (for example, moving)
or is it ready to accept new commands?

Implements static pauses.
-}
isMotorReady :: [OutputPort] -> NXT [Bool]
isMotorReady ports = do
  mapM_ (\port -> motorControlSend $ '3' : port) ports''
  liftIO $ threadDelay (10 * 1000) -- 10 ms
  replies <- mapM (\_ -> motorControlReceive) ports''
  liftIO $ threadDelay (10 * 1000) -- 10 ms
  let replies' = map (\[p, r] -> ([p], r == '1')) replies
  return $ map (fromJust . (`lookup` replies')) ports'
    where ports' = map (show . fromEnum) ports
          ports'' = nub ports'

{-|
Interface to @CLASSIC_MOTORCMD@ command which is very similar to 'setOutputState' but better interacts with @MotorControl@.

Requires dynamic\/minimal pauses.
-}
classicMotorCmd :: [OutputPort] -> OutputPower -> TachoLimit -> SpeedRegulation -> NXT ()
classicMotorCmd ports power limit regulation = motorControlSend message
  where message = "4" ++ fromPorts ports ++ fromPower power ++ fromLimit limit ++ regulation'
        regulation' | regulation = "1"
                    | otherwise  = "0"

data MotorControlMode =
    HoldBrake -- ^ Keeps active brake on after the end of the movement.
  | SpeedRegulation -- ^ In a case of a load on the motor adapt motor power to retain the same speed. Really works only if there is
                    -- room for that (not that motor is already running at the maximum power).
  | SmoothStart -- ^ Smoothly starts the movement.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

{-|
Starts @MotorControl22.rxe@ program on the NXT brick. Program has to be uploaded/available on the NXT brick.
-}
startMotorControlProgram :: NXT ()
startMotorControlProgram = ensureStartProgram programFilename

{-|
Stops current running program on the NXT brick. Probably this means @MotorControl22.rxe@ program on the NXT brick.
-}
stopMotorControlProgram :: NXT ()
stopMotorControlProgram = stopProgram

-- Sends a message to MotorControl program on the NXT brick
motorControlSend :: String -> NXT ()
motorControlSend = messageWrite Inbox1

-- Reads a message from MotorControl program on the NXT brick
motorControlReceive :: NXT String
motorControlReceive = ensureMessageRead RemoteInbox0 True

fromPorts :: [OutputPort] -> String
fromPorts ports | length ports' == 1 = show . fromEnum . head $ ports'
                | otherwise          = case ports' of
                                         [A, B]    -> "3"
                                         [A, C]    -> "4"
                                         [B, C]    -> "5"
                                         [A, B, C] -> "6"
                                         _         -> throw $ PatternMatchFail "fromPorts"
  where ports' = sort . nub $ ports

fromPower :: OutputPower -> String
fromPower power | (-100) <= power && power <= 100 = printf "%03d" $ if power < 0 then 100 + abs power else power
                | otherwise                       = throw $ PatternMatchFail "fromPower"

fromLimit :: TachoLimit -> String
fromLimit limit | 0 <= limit && limit <= 999999 = printf "%06d" limit
                | otherwise                     = throw $ PatternMatchFail "fromLimit"

{-|
Should in a case of a load on the motor its power be adapted to retain the same speed? Really works only if there is room for that
(not that motor is already running at the maximum power).
-}
type SpeedRegulation = Bool
