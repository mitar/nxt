module NXT.Capture (initCapture, terminateCapture, getLastRobotPosition) where

import Control.Exception
import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as C
import System.IO
import System.IO.Error
import System.Process

import NXT.CaptureTypes
import NXT.CaptureUtils
import NXT.NXT
import NXT.NXTTypes

-- TODO Move to configuration file
enableCapture :: Bool
enableCapture = False

initCapture :: NXT ()
initCapture = do
  if not enableCapture
    then return ()
    else do
      -- TODO Move to configuration file
      (_, Just captureOut, _, capturePid) <- io $ createProcess (proc "/home/natrix/files/capture" []){ cwd = Just "/home/natrix/files", std_out = CreatePipe, std_err = Inherit, close_fds = True }
      processRunning <- io $ getProcessExitCode capturePid
      case processRunning of
        Just _ -> fail "Capture process could not start"
        _      -> do
          io $ hSetBuffering captureOut NoBuffering
          let capacity = 10 * maxRobotDescLength
          modify (\s -> s { capture = Just (Capture captureOut capturePid (CaptureBuffer capacity C.empty)) })

terminateCapture :: NXT ()
terminateCapture = do
  c <- gets capture
  case c of
    Just (Capture _ capturePid _) -> do
      modify (\s -> s { capture = Nothing })
      io $ terminateProcess capturePid
    _                                                     -> return ()

getLastRobotPosition :: NXT (Maybe Robot)
getLastRobotPosition = do
  c <- gets capture
  case c of
    Nothing                                            -> fail "No capture process"
    Just (Capture captureOut capturePid captureBuffer) -> do
      processRunning <- io $ getProcessExitCode capturePid
      case processRunning of
        Just _ -> captureProcessExited
        _      -> do
          ret <- io $ tryJust (guard . isEOFError) $ slurpInput captureOut captureBuffer
          case ret of
            Left _                           -> captureProcessExited -- EOF
            Right (positions, currentBuffer) -> do
              modify (\s -> s { capture = Just (Capture captureOut capturePid currentBuffer) })
              if null positions
                then return Nothing
                else return $ Just $ head . head $ positions -- positions are stored as lists in the reverse order so (head . head) is the last position from capture process

captureProcessExited :: NXT a
captureProcessExited = do
  terminateCapture 
  fail "Capture process exited"
