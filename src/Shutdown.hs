module Shutdown where

import Control.Exception
import Control.Monad.State

import qualified NXT.NXT as NXT

shutdown :: IO ()
shutdown = bracket NXT.initialize NXT.terminate (evalStateT NXT.shutdown)
