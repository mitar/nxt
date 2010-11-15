{-# LANGUAGE DeriveDataTypeable #-}

module Robotics.NXT.Errors (
  failNXT,
  failNXT',
  NXTException(..)
) where

import Control.Exception
import Data.Typeable
import Data.Word

-- Possible error codes and their descriptions
-- Described in Lego Mindstorms NXT Bluetooth Developer Kit:
--  Appendix 1 - Communication protocol
--  Appendix 2 - Direct commands

failNXT :: String -> Word8 -> IO a
failNXT msg 0x20 = throwIO . NXTException $ msg ++ ": Pending communication transaction in progress"
failNXT msg 0x40 = throwIO . NXTException $ msg ++ ": Specified mailbox queue is empty"
failNXT msg 0x81 = throwIO . NXTException $ msg ++ ": No more handles"
failNXT msg 0x82 = throwIO . NXTException $ msg ++ ": No space"
failNXT msg 0x83 = throwIO . NXTException $ msg ++ ": No more files"
failNXT msg 0x84 = throwIO . NXTException $ msg ++ ": End of file expected"
failNXT msg 0x85 = throwIO . NXTException $ msg ++ ": End of file"
failNXT msg 0x86 = throwIO . NXTException $ msg ++ ": Not a linear file"
failNXT msg 0x87 = throwIO . NXTException $ msg ++ ": File not found"
failNXT msg 0x88 = throwIO . NXTException $ msg ++ ": Handle all ready closed"
failNXT msg 0x89 = throwIO . NXTException $ msg ++ ": No linear space"
failNXT msg 0x8A = throwIO . NXTException $ msg ++ ": Undefined error"
failNXT msg 0x8B = throwIO . NXTException $ msg ++ ": File is busy"
failNXT msg 0x8C = throwIO . NXTException $ msg ++ ": No write buffers"
failNXT msg 0x8D = throwIO . NXTException $ msg ++ ": Append not possible"
failNXT msg 0x8E = throwIO . NXTException $ msg ++ ": File is full"
failNXT msg 0x8F = throwIO . NXTException $ msg ++ ": File exists"
failNXT msg 0x90 = throwIO . NXTException $ msg ++ ": Module not found"
failNXT msg 0x91 = throwIO . NXTException $ msg ++ ": Out of boundary"
failNXT msg 0x92 = throwIO . NXTException $ msg ++ ": Illegal file name"
failNXT msg 0x93 = throwIO . NXTException $ msg ++ ": Illegal handle"
failNXT msg 0xBD = throwIO . NXTException $ msg ++ ": Request failed (i.e. specified file not found)"
failNXT msg 0xBE = throwIO . NXTException $ msg ++ ": Unknown command opcode"
failNXT msg 0xBF = throwIO . NXTException $ msg ++ ": Insane packet"
failNXT msg 0xC0 = throwIO . NXTException $ msg ++ ": Data contains out-of-range values"
failNXT msg 0xDD = throwIO . NXTException $ msg ++ ": Communication bus error"
failNXT msg 0xDE = throwIO . NXTException $ msg ++ ": No free memory in communication buffer"
failNXT msg 0xDF = throwIO . NXTException $ msg ++ ": Specified channel/connection is not valid"
failNXT msg 0xE0 = throwIO . NXTException $ msg ++ ": Specified channel/connection not configured or busy"
failNXT msg 0xEC = throwIO . NXTException $ msg ++ ": No active program"
failNXT msg 0xED = throwIO . NXTException $ msg ++ ": Illegal size specified"
failNXT msg 0xEE = throwIO . NXTException $ msg ++ ": Illegal mailbox queue ID specified"
failNXT msg 0xEF = throwIO . NXTException $ msg ++ ": Attempted to access invalid field of a structure"
failNXT msg 0xF0 = throwIO . NXTException $ msg ++ ": Bad input or output specified"
failNXT msg 0xFB = throwIO . NXTException $ msg ++ ": Insufficient memory available"
failNXT msg 0xFF = throwIO . NXTException $ msg ++ ": Bad arguments"
failNXT msg 0x00 = throwIO . NXTException $ msg -- some guard (restriction) failed?
failNXT msg _    = throwIO . NXTException $ msg -- invalid error code?

failNXT' :: String -> IO a
failNXT' msg = throwIO . NXTException $ msg

data (Show a, Typeable a) => NXTException a = NXTException a deriving (Show, Typeable)

instance (Show a, Typeable a) => Exception (NXTException a)
