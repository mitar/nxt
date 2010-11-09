module NXT.Remote (
  sendRemoteCommand,
  startRemoteProgram,
  stopRemoteProgram,
  motorControlSend,
  motorControlReceive
) where

import Control.Exception
import Control.Monad.State
import Data.Maybe
import Text.Printf

import NXT.NXT
import NXT.Types

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

startRemoteProgram :: NXT ()
startRemoteProgram = do
  stopAndWait
  startAndWait
    where stopAndWait = do
            stopProgramConfirm
            name <- getCurrentProgramName
            unless (isNothing name) stopAndWait
          startAndWait = do
            startProgramConfirm "remote.rxe"
            name <- getCurrentProgramName
            unless (isJust name) startAndWait

stopRemoteProgram :: NXT ()
stopRemoteProgram = stopProgram

-- Sends a message to MotorControl program on NXT
-- Be careful to obey required pauses between commands (threadDelay is your friend)
motorControlSend :: String -> NXT ()
motorControlSend = messageWrite Inbox1

-- Reads a message from MotorControl program on NXT
-- Be careful to obey required pauses between commands (threadDelay is your friend)
motorControlReceive :: NXT String
motorControlReceive = messageRead RemoteInbox0 True
