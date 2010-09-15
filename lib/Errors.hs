module NXT.Errors where

import Data.Word

-- Possible error codes and their descriptions
-- Described in Lego Mindstorms NXT Bluetooth Developer Kit:
--  Appendix 1 - Communication protocol
--  Appendix 2 - Direct commands

failNXT :: Monad m => String -> Word8 -> m a
failNXT msg 0x20 = fail $ msg ++ ": Pending communication transaction in progress"
failNXT msg 0x40 = fail $ msg ++ ": Specified mailbox queue is empty"
failNXT msg 0x81 = fail $ msg ++ ": No more handles"
failNXT msg 0x82 = fail $ msg ++ ": No space"
failNXT msg 0x83 = fail $ msg ++ ": No more files"
failNXT msg 0x84 = fail $ msg ++ ": End of file expected"
failNXT msg 0x85 = fail $ msg ++ ": End of file"
failNXT msg 0x86 = fail $ msg ++ ": Not a linear file"
failNXT msg 0x87 = fail $ msg ++ ": File not found"
failNXT msg 0x88 = fail $ msg ++ ": Handle all ready closed"
failNXT msg 0x89 = fail $ msg ++ ": No linear space"
failNXT msg 0x8A = fail $ msg ++ ": Undefined error"
failNXT msg 0x8B = fail $ msg ++ ": File is busy"
failNXT msg 0x8C = fail $ msg ++ ": No write buffers"
failNXT msg 0x8D = fail $ msg ++ ": Append not possible"
failNXT msg 0x8E = fail $ msg ++ ": File is full"
failNXT msg 0x8F = fail $ msg ++ ": File exists"
failNXT msg 0x90 = fail $ msg ++ ": Module not found"
failNXT msg 0x91 = fail $ msg ++ ": Out of boundary"
failNXT msg 0x92 = fail $ msg ++ ": Illegal file name"
failNXT msg 0x93 = fail $ msg ++ ": Illegal handle"
failNXT msg 0xBD = fail $ msg ++ ": Request failed (i.e. specified file not found)"
failNXT msg 0xBE = fail $ msg ++ ": Unknown command opcode"
failNXT msg 0xBF = fail $ msg ++ ": Insane packet"
failNXT msg 0xC0 = fail $ msg ++ ": Data contains out-of-range values"
failNXT msg 0xDD = fail $ msg ++ ": Communication bus error"
failNXT msg 0xDE = fail $ msg ++ ": No free memory in communication buffer"
failNXT msg 0xDF = fail $ msg ++ ": Specified channel/connection is not valid"
failNXT msg 0xE0 = fail $ msg ++ ": Specified channel/connection not configured or busy"
failNXT msg 0xEC = fail $ msg ++ ": No active program"
failNXT msg 0xED = fail $ msg ++ ": Illegal size specified"
failNXT msg 0xEE = fail $ msg ++ ": Illegal mailbox queue ID specified"
failNXT msg 0xEF = fail $ msg ++ ": Attempted to access invalid field of a structure"
failNXT msg 0xF0 = fail $ msg ++ ": Bad input or output specified"
failNXT msg 0xFB = fail $ msg ++ ": Insufficient memory available"
failNXT msg 0xFF = fail $ msg ++ ": Bad arguments"
failNXT msg 0x00 = fail msg -- Some guard (restriction) failed?
failNXT msg _    = fail msg -- Invalid error code?
