module NXT.Capture (initCapture, terminateCapture, getLastRobotPosition, waitForRobotPosition, waitForRobotPosition') where

import Control.Exception
import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Time.Clock.POSIX
import System.IO
import System.IO.Error
import System.Process

import NXT.CaptureTypes
import NXT.CaptureUtils
import NXT.NXT
import NXT.NXTTypes

-- TODO Move to configuration file
enableCapture :: Bool
enableCapture = True

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
                else do
                  let Robot x y d ct pt _ = head . head $ positions -- positions are stored as lists in the reverse order so (head . head) is the last position from capture process
                  currentTime <- io $ getPOSIXTime
                  return $ Just $ Robot x y d ct pt currentTime

waitForRobotPosition :: NXT ()
waitForRobotPosition = do
  _ <- waitForRobotPosition' (-1)
  return ()

waitForRobotPosition' :: Int -> NXT Bool
waitForRobotPosition' timeout = do
  c <- gets capture
  case c of
    Nothing                                -> fail "No capture process"
    Just (Capture captureOut capturePid _) -> do
      processRunning <- io $ getProcessExitCode capturePid
      case processRunning of
        Just _ -> captureProcessExited
        _      -> do
          ret <- io $ tryJust (guard . isEOFError) $ hWaitForInput captureOut timeout
          case ret of
            Left _               -> captureProcessExited -- EOF
            Right inputAvailable -> return inputAvailable

captureProcessExited :: NXT a
captureProcessExited = do
  terminateCapture 
  fail "Capture process exited"
