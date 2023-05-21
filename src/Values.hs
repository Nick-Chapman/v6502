
module Values
  ( Bit(..)
  , Byte(..)
  , Addr(..)
  , bitsToByte, splitB
  , bitsToAddr
  ) where

import Data.Word (Word8,Word16)
import Text.Printf (printf)

data Bit = Bit Bool
instance Show Bit where show (Bit bool) = if bool then "1" else "0"

data Byte = Byte Word8
instance Show Byte where show (Byte w8) = printf "%02x" w8

data Addr = Addr Word16
instance Show Addr where show (Addr w16) = printf "%04x" w16

bitsToByte :: [Bool] -> Byte -- msb->lsb
bitsToByte bs =
  if length bs /= 8 then error "bitsToByte" else
    Byte (foldl (\acc b -> 2*acc+(if b then 1 else 0)) (0::Word8) bs)

splitB :: Byte -> [Bool]
splitB (Byte w8) =
  reverse (take 8 (bitsOf w8))
  where
    bitsOf :: Word8 -> [Bool]
    bitsOf n = ((n `mod` 2) == 1) : bitsOf (n `div` 2)

bitsToAddr :: [Bool] -> Addr -- msb->lsb
bitsToAddr bs =
  if length bs /= 16 then error "bitsToAddr" else
    Addr (foldl (\acc b -> 2*acc+(if b then 1 else 0)) (0::Word16) bs)
