{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Robotics.NXT.Internals where

import Control.Monad.State
import Data.Time.Clock.POSIX
import System.IO

import Robotics.NXT.Externals

{-|
Monad which encompasses interface to the NXT brick.
-}
newtype NXT a = NXT (StateT NXTInternals IO a) deriving (Monad, MonadIO, Functor, MonadFix) -- NXT monad

{-|
A token used for exposed internal functions.
-}
data NXTInternals = NXTInternals {
    nxthandle :: Handle, -- a handle of the opened serial port
    address :: Maybe BTAddress,
    modules :: [(ModuleName, ModuleInfo)], -- modules info
    sleeptime :: Maybe Duration, -- sleep time limit in seconds
    lastkeepalive :: Maybe POSIXTime -- last time keep alive has been sent
  }

instance Show NXTInternals where
  show _ = "NXTInternals"

{-|
Runs a computation in a context of a given 'NXTInternals' token, returning a value and a new token.
-}
runNXT :: NXT a -> NXTInternals -> IO (a, NXTInternals)
runNXT (NXT action) = runStateT action

{-|
Runs a computation in a context of a given 'NXTInternals' token, returning just a new token.
-}
execNXT :: NXT a -> NXTInternals -> IO NXTInternals
execNXT (NXT action) = execStateT action

modifyNXT :: (NXTInternals -> NXTInternals) -> NXT ()
modifyNXT f = NXT (modify f)

getsNXT :: (NXTInternals -> a) -> NXT a
getsNXT f = NXT (gets f)
