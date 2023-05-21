
module CBM (main) where

import Data.Map (Map)
import EmuState (showState)
import GetLogic (Version(..),getLogic)
import Sim2 (Sim(..),simGivenLogic)
import Values (Addr,Byte,loadBytes)
import qualified Data.Map as Map

main :: Version -> Int -> IO ()
main version n = do
  let path = "../perfect6502/rom/cbmbasic.bin"
  let expectedSize = 17591
  image0 <- loadImage 0xA000 path expectedSize

  -- comment copied from perfect6502
  {-
   * cbmbasic scribbles over 0x01FE/0x1FF, so we can't start
   * with a stackpointer of 0 (which seems to be the state
   * after a RESET), so RESET jumps to 0xF000, which contains
   * a JSR to the actual start of cbmbasic
   -}
  let image = foldl writeMem image0
        [ (0xf000, 0x20)
        , (0xf001, 0x94)
        , (0xf002, 0xE3)

        , (0xfffc, 0x00)
        , (0xfffd, 0xf0)
        ]

  logic <- getLogic version
  --print (Summary logic)
  let sim = simGivenLogic logic
  simWithImage n image sim
  pure ()


simWithImage :: Int -> Image -> Sim -> IO ()
simWithImage max = loop 0
  where
    loop :: Int -> Image -> Sim -> IO ()
    loop i image sim = do
     case sim of
      Stabilization _iopt sim -> do
        --printf "stabilization in %s\n" (show _iopt)
        loop i image sim
      NewState state sim -> do
        putStrLn (showState i state)
        if (i==max) then pure () else loop (i+1) image sim
      Decide _addr _kind sim -> do
        --print ("Decide:",i,_kind)
        loop i image sim
      ReadMem a f -> do
        let b = readMem image a
        --printf "r: %s --> %s\n" (show a) (show b)
        loop i image (f b)
      WriteMem a b sim -> do
        --printf "W: %s <-- %s\n" (show a) (show b)
        loop i (writeMem image (a,b)) sim

data Image = Image { m :: Map Addr Byte }

loadImage :: Addr -> FilePath -> Int  -> IO Image
loadImage offset path expectedSize  = do
  bytes <- loadBytes path
  if length bytes /= expectedSize then error (show ("loadImage",expectedSize,length bytes)) else
    pure Image { m = Map.fromList [ (offset+a,b) | (a,b) <- zip [0::Addr ..] bytes ] }

readMem :: Image -> Addr -> Byte
readMem Image{m} a = maybe 0 id $ (Map.lookup a m)

writeMem :: Image -> (Addr,Byte) -> Image
writeMem Image{m} (a,b) = Image (Map.insert a b m)
