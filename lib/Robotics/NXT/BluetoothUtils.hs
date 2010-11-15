{-# LANGUAGE ForeignFunctionInterface #-}
{-# CFILES ffi/blue.c #-}

module Robotics.NXT.BluetoothUtils (
  bluetoothRSSI,
  bluetoothLinkQuality,
  bluetoothAddress
) where

import Control.Exception
import Control.Monad.State
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Robotics.NXT.Errors
import Robotics.NXT.Protocol
import Robotics.NXT.Types

-- Foreign function call for C function which returns RSSI Bluetooth value of a connection to a given Bluetooth address
foreign import ccall unsafe "rssi" rssi :: CString -> IO CInt

-- Foreign function call for C function which returns link quality Bluetooth value of a connection to a given Bluetooth address
foreign import ccall unsafe "lq" lq :: CString -> IO CInt

-- As defined in blue.h
blueError :: Int
blueError = 1000
blueNotConnected :: Int
blueNotConnected = 1001
blueNotSupported :: Int
blueNotSupported = 1002

bluetoothRSSI :: NXT Int
bluetoothRSSI = do
  addr <- bluetoothAddress
  bluetoothRSSIAddr addr

bluetoothRSSIAddr :: BTAddress -> NXT Int
bluetoothRSSIAddr addr = do
  ret <- liftIO $ withCString addr rssi
  let ret' = fromIntegral ret
  case ret' of
    _ | ret' == blueError        -> liftIO $ throwIO $ NXTException "Could not get connection's RSSI"
      | ret' == blueNotConnected -> liftIO $ throwIO $ NXTException "Connection not established"
      | ret' == blueNotSupported -> liftIO $ throwIO $ NXTException "Not supported on this system"
      | otherwise                -> return ret'

bluetoothLinkQuality :: NXT Int
bluetoothLinkQuality = do
  addr <- bluetoothAddress
  bluetoothLinkQualityAddr addr

bluetoothLinkQualityAddr :: BTAddress -> NXT Int
bluetoothLinkQualityAddr addr = do
  ret <- liftIO $ withCString addr lq
  let ret' = fromIntegral ret
  case ret' of
    _ | ret' == blueError        -> liftIO $ throwIO $ NXTException "Could not get connection's link quality"
      | ret' == blueNotConnected -> liftIO $ throwIO $ NXTException "Connection not established"
      | ret' == blueNotSupported -> liftIO $ throwIO $ NXTException "Not supported on this system"
      | otherwise                -> return ret'

bluetoothAddress :: NXT BTAddress
bluetoothAddress = do
  addr <- gets address
  case addr of
    Just a  -> return a
    Nothing -> do
      getDeviceInfo
      (Just a) <- gets address
      return a
