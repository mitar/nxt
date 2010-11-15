module Robotics.NXT.Data (
  fromUByte,
  fromUWord,
  fromULong,
  fromSByte,
  fromSWord,
  fromSLong,
  dataToString,
  dataToString0,
  toUByte,
  toUWord,
  toULong,
  toSByte,
  toSWord,
  toSLong,
  stringToData,
  stringToData0,
  nameToData,
  messageToData
) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List
import Data.Word
import Control.Exception

-- Converts a list of bytes to an unsigned numeric value
dataToInt :: Integral a => [Word8] -> a -- least significant byte first
dataToInt = foldr addByte 0x00
  where addByte x y = y' * 0x100 + x'
          where x' = fromIntegral x
                y' = fromIntegral y

fromUByte :: Integral a => [Word8] -> a -- one byte, unsigned
fromUByte ws@[_] = dataToInt ws
fromUByte _      = throw $ PatternMatchFail "fromUByte"

fromUWord :: Integral a => [Word8] -> a -- two bytes, unsigned, least significant byte first
fromUWord ws@[_, _] = dataToInt ws
fromUWord _         = throw $ PatternMatchFail "fromUWord"

fromULong :: Integral a => [Word8] -> a -- four bytes, unsigned, least significant byte first
fromULong ws@[_, _, _, _] = dataToInt ws
fromULong _               = throw $ PatternMatchFail "fromULong"

fromSByte :: Integral a => [Word8] -> a -- one byte, signed
fromSByte ws@[b] | b <= 0x7F = dataToInt ws
                 | otherwise = negate . (-) 0x100 . dataToInt $ ws
fromSByte _                  = throw $ PatternMatchFail "fromSByte"

fromSWord :: Integral a => [Word8] -> a -- two bytes, signed, least significant byte first
fromSWord ws@[_, b] | b <= 0x7F = dataToInt ws
                    | otherwise = negate . (-) 0x10000 . dataToInt $ ws
fromSWord _                     = throw $ PatternMatchFail "fromSWord"

fromSLong :: Integral a => [Word8] -> a -- four bytes, signed, least significant byte first
fromSLong ws@[_, _, _, b] | b <= 0x7F = dataToInt ws
                          | otherwise = negate . (-) 0x100000000 . dataToInt $ ws
fromSLong _                           = throw $ PatternMatchFail "fromSLong"

-- Converts a null-terminated list of bytes to a string
dataToString0 :: [Word8] -> String
dataToString0 = dataToString . takeWhile (/= 0x00)

dataToString :: [Word8] -> String
dataToString = C.unpack . B.pack

-- Converts a numeric value to list of bytes
-- In a case of a negative number it produces an infinite list
intToData :: Integral a => a -> [Word8] -- least significant byte first
intToData 0x00 = [0x00]
intToData x    = unfoldr getByte x
  where getByte 0x00 = Nothing
        getByte y    = Just (fromIntegral $ y `mod` 0x100, y `div` 0x100)

toUByte :: Integral a => a -> [Word8] -- one byte, unsigned
toUByte x | x >= 0x00 && x <= 0xFF = intToData x
          | otherwise              = throw . PatternMatchFail $ "toUByte: " ++ show x

toUWord :: Integral a => a -> [Word8] -- two bytes, unsigned, least significant byte first
toUWord x | x >= 0x00 && x <= 0xFFFF = take 2 . flip (++) (repeat 0x00) . intToData $ x
          | otherwise                = throw . PatternMatchFail $ "toUWord: " ++ show x

toULong :: Integral a => a -> [Word8] -- four bytes, unsigned, least significant byte first
toULong x | x' >= 0x00 && x' <= 0xFFFFFFFF = take 4 . flip (++) (repeat 0x00) . intToData $ x'
          | otherwise                      = throw . PatternMatchFail $ "toULong: " ++ show x
  where x' = fromIntegral x :: Integer

toSByte :: Integral a => a -> [Word8] -- one byte, signed
toSByte x | x >= (-0x80) && x < 0x00 = intToData $ 0x100 + x
          | x >= 0x00 && x <= 0x7F   = intToData x
          | otherwise                = throw . PatternMatchFail $ "toSByte: " ++ show x

toSWord :: Integral a => a -> [Word8] -- two bytes, signed, least significant byte first
toSWord x | x >= (-0x8000) && x < 0x00 = take 2 . flip (++) (repeat 0x00) . intToData $ 0x10000 + x
          | x >= 0x00 && x <= 0x7FFF   = take 2 . flip (++) (repeat 0x00) . intToData $ x
          | otherwise                  = throw . PatternMatchFail $ "toSWord: " ++ show x

toSLong :: Integral a => a -> [Word8] -- four bytes, signed, least significant byte first
toSLong x | x' >= (-0x80000000) && x' < 0x00 = take 4 . flip (++) (repeat 0x00) . intToData $ 0x100000000 + x'
          | x' >= 0x00 && x' <= 0x7FFFFFFF   = take 4 . flip (++) (repeat 0x00) . intToData $ x'
          | otherwise                        = throw . PatternMatchFail $ "toSLong: " ++ show x
  where x' = fromIntegral x :: Integer

-- Converts a string to a null-terminated list of bytes
stringToData0 :: String -> [Word8]
stringToData0 = stringToData . flip (++) "\0"

stringToData :: String -> [Word8]
stringToData = B.unpack . C.pack

-- Converts a name to a null-terminated list of bytes
nameToData :: String -> [Word8]
nameToData = stringToData0 . take 19 . flip (++) (repeat '\0')

-- Converts a message to a null-terminated list of bytes
messageToData :: String -> [Word8]
messageToData = stringToData0 . take 58
