{-|
A simple message-based control of the NXT brick via remotely executed program. The main goal of this approach is to reduce latency
otherwise encountered when using NXT Bluetooth communication. Namely, changing the direction of communication takes around 30 ms.
This means that one cycle of requesting motor status, receiving data and then sending motor control update takes at least 60 ms (it
takes two changes of communication direction). And this is especially a problem if you want to control multiple motors at the same
time (and built-in synchronize mechanism is not good enough for you).

One solution to this problem is that Bluetooth is used only for communication in one direction and an additional program on the NXT
brick is checking motor status and correcting possible errors. Of course this also means that information about motor position have
to be obtained in some other way: by predicting from sent commands, or measuring/probing with regular NXT commands when there is time
for that (and latency at that time is not a problem), or by using some external sensors attached to the controlling computer and not
to the NXT brick (camera tracking system for example).

Use NBC (<http://bricxcc.sourceforge.net/nbc/>) to compile basic NXC code (@remote.nxc@) included with this module into @remote.rxe@
NXT program (or use already compiled version) and then upload it with @nxt-upload@ program. This module will then run that program
and sends it messages to control the NXT brick. Because commands will be executed and controlled directly on the NXT brick less
latency and more powerful control is possible.

Check "Robotics.NXT.MotorControl" for another (more precise but more complex) approach to message-based control. It would be great to
one day combine @MotorControl@'s precision with API (especially interruptability) of @remote.nxc@.
-}

module Robotics.NXT.Remote (
  -- * Initialization
  startRemoteProgram,
  stopRemoteProgram,
  -- * Control
  sendRemoteCommand,
  RemoteCommand(..),
  RemoteCommandType(..)
) where

import Control.Exception
import Text.Printf

import Robotics.NXT

programFilename :: FileName
programFilename = "remote.rxe"

data RemoteCommandType =
    MoveFor OutputPower TachoLimit -- ^ Move specified motors for a given number of degrees at a given speed.
  | SetTo OutputPower TachoCount -- ^ Set specified motors' position to a given offset in degrees from a zero position (it is assumed
                                 -- that motors are at zero position at @remote.rxe@ program start, you can use 'resetMotorPosition'
                                 -- to assure that, but probably not needed as the NXT brick resets things at program start) at a given speed.
                                 -- Probably not a good idea to mix 'SetTo' and 'MoveFor' on the same motor as 'MoveFor' resets
                                 -- position.
  deriving (Eq, Ord, Read, Show)
data RemoteCommand =
    RemoteCommand [OutputPort] RemoteCommandType -- ^ Data type of remote command for specified output port(s).
  deriving (Eq, Ord, Read, Show)

{-|
Sends a command to the @remote.rxe@ program. Commands can be interrupted (or supplemented, for other motors) immediately by a next command.
-}
sendRemoteCommand :: RemoteCommand -> NXT ()
sendRemoteCommand = messageWrite Inbox1 . encodeRemoteCommand

encodeRemoteCommand :: RemoteCommand -> String
encodeRemoteCommand (RemoteCommand ports command) =
  case command of
    (MoveFor power limit) | (-100) <= power && power <= 100 && 0 <= limit && limit <= 999999 -> "6" ++ ports' ++ power' ++ limit'
                          | otherwise                                                        -> throw $ PatternMatchFail "encodeRemoteCommand"
                              where power' = printf "%03d" $ power + 100
                                    limit' = printf "%06d" limit
    (SetTo power count) | 0 <= power && power <= 100 && (-99999) <= count && count <= 99999 -> "7" ++ ports' ++ power' ++ count'
                        | otherwise                                                         -> throw $ PatternMatchFail "encodeRemoteCommand"
                            where power' = printf "%03d" $ power + 100
                                  count' = printf "%+06d" count
  where ports' = show . sum . zipWith (*) [1, 2, 4] . map (fromEnum . flip elem ports) $ [A ..]

{-|
Starts @remote.rxe@ program on the NXT brick. Program has to be uploaded/available on the NXT brick.
-}
startRemoteProgram :: NXT ()
startRemoteProgram = ensureStartProgram programFilename

{-|
Stops current running program on the NXT brick. Probably this means @remote.rxe@ program on the NXT brick.
-}
stopRemoteProgram :: NXT ()
stopRemoteProgram = stopProgram
