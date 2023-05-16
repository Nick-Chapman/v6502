
module Dormann (main) where

import Control.Monad (when)
import Misc (loadBytes)
import Data.Map (Map)
import Data.Word (Word8,Word16)
import Sim2 (Version,Sim(..),Addr(..),Byte(..))
import qualified Data.Map as Map
import qualified Sim2

main :: Version -> IO ()
main v = do
  print "dormannn-start"
  let path = "../6502_65C02_functional_tests/bin_files/6502_functional_test.bin"
  image0 <- loadImage path
  let image = foldl writeMem image0 [ (Addr 0xfffc, Byte 0x00)
                                    , (Addr 0xfffd, Byte 0x04)
                                    ]
  sim <- Sim2.theSim v
  simWithImage image sim
  print "dormannn-end"


simWithImage :: Image -> Sim -> IO ()
simWithImage = loop 0
  where
    loop :: Int -> Image -> Sim -> IO ()
    loop i image sim = {-if i==100 then pure () else-} case sim of
      Stabilization _iopt sim -> do
        --print ("stabilization",_iopt)
        loop i image sim
      NewState _state sim -> do
        --print (StateSum i _state)
        --print "NEW-STATE"
        loop i image sim
      Decide _addr _kind sim -> do
        --print ("Decide:",i,_kind)
        loop i image sim
      ReadMem a f -> do
        let b = readMem image a
        when (i `mod` 100 ==0) $ print ("read",i, a,"-->",b)
        loop (i+1) image (f b)
      WriteMem a b sim -> do
        print ("WRITE",i, a,"<--",b)
        loop (i+1) (writeMem image (a,b)) sim



data Image = Image { m :: Map Word16 Word8 }

loadImage :: FilePath -> IO Image
loadImage path = do
  bytes <- loadBytes path
  if length bytes /= 256*256 then undefined else
    pure Image { m = Map.fromList [ (a,b) | (a,b) <- zip [0..] bytes ] }

readMem :: Image -> Addr -> Byte
readMem Image{m} (Addr w16) = maybe undefined Byte $ (Map.lookup w16 m)

writeMem :: Image -> (Addr,Byte) -> Image
writeMem Image{m} (Addr a, Byte b) = Image (Map.insert a b m)
