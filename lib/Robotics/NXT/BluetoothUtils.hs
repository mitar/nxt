{-# LANGUAGE ForeignFunctionInterface, CPP #-}
{-# CFILES ffi/blue.c #-}

module Robotics.NXT.BluetoothUtils (
  -- * Bluetooth utils
  -- | `getDeviceInfo` returns zero for Bluetooth signal strength as this is not implemented in current NXT firmware versions. 
  -- Here are functions which retrieve that from a host (computer) Bluetooth stack.
  bluetoothRSSI,
  bluetoothLinkQuality
) where

import Control.Exception
import Control.Monad.State
import Foreign.C.String
import Foreign.C.Types

import Robotics.NXT.Errors
import Robotics.NXT.Protocol
import Robotics.NXT.Types
import Robotics.NXT.Internals

#ifdef linux_HOST_OS
-- Foreign function call for C function which returns RSSI Bluetooth value of a connection to a given Bluetooth address
foreign import ccall unsafe "rssi" rssi :: CString -> IO CInt

-- Foreign function call for C function which returns link quality Bluetooth value of a connection to a given Bluetooth address
foreign import ccall unsafe "lq" lq :: CString -> IO CInt
#endif

-- As defined in blue.h
blueError :: Int
blueError = 1000
blueNotConnected :: Int
blueNotConnected = 1001

{-|
Gets received signal strength indicator (RSSI) of the Bluetooth connection to the NXT brick.

Currently supported only on Linux. It throws a 'NXTException' otherwise.
-}
bluetoothRSSI :: NXT Int
bluetoothRSSI = do
  addr <- bluetoothAddress
  bluetoothRSSIAddr addr

bluetoothRSSIAddr :: BTAddress -> NXT Int
bluetoothRSSIAddr addr = do
#ifdef linux_HOST_OS
  ret <- liftIO $ withCString addr rssi
  let ret' = fromIntegral ret
  case ret' of
    _ | ret' == blueError        -> liftIO $ throwIO $ NXTException "Could not get connection's RSSI"
      | ret' == blueNotConnected -> liftIO $ throwIO $ NXTException "Connection not established"
      | otherwise                -> return ret'
#else
    liftIO $ throwIO $ NXTException "Not supported on this system"
#endif

{-|
Gets link quality of the Bluetooth connection to the NXT brick.

Currently supported only on Linux. It throws a 'NXTException' otherwise.
-}
bluetoothLinkQuality :: NXT Int
bluetoothLinkQuality = do
  addr <- bluetoothAddress
  bluetoothLinkQualityAddr addr

bluetoothLinkQualityAddr :: BTAddress -> NXT Int
bluetoothLinkQualityAddr addr = do
#ifdef linux_HOST_OS
  ret <- liftIO $ withCString addr lq
  let ret' = fromIntegral ret
  case ret' of
    _ | ret' == blueError        -> liftIO $ throwIO $ NXTException "Could not get connection's link quality"
      | ret' == blueNotConnected -> liftIO $ throwIO $ NXTException "Connection not established"
      | otherwise                -> return ret'
#else
  liftIO $ throwIO $ NXTException "Not supported on this system"
#endif

bluetoothAddress :: NXT BTAddress
bluetoothAddress = do
  addr <- getsNXT address
  case addr of
    Just a  -> return a
    Nothing -> do
      _ <- getDeviceInfo
      (Just a) <- getsNXT address
      return a
