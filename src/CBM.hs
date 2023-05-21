
module CBM (main) where

import Assigns (NodeId)
import Data.Map (Map)
import Data.Word (Word8,Word16)
import GetLogic (Version(..),getLogic)
import Misc (loadBytes)
import NodeNames (ofName)
import Sim2 (Sim(..),State,Addr(..),Byte(..),Bit(..),simGivenLogic,lookState)
import Text.Printf (printf)
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
        [ (Addr 0xf000, Byte 0x20)
        , (Addr 0xf001, Byte 0x94)
        , (Addr 0xf002, Byte 0xE3)

        , (Addr 0xfffc, Byte 0x00)
        , (Addr 0xfffd, Byte 0xf0)
        ]

  sim <- theSim version
  simWithImage n image sim
  pure ()


theSim :: Version -> IO Sim
theSim v = do
  logic <- getLogic v
  --print (Summary logic)
  pure (simGivenLogic logic)



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
        ppState i state
        if (i==max) then pure () else loop (i+1) image sim
      Decide _addr _kind sim -> do
        --print ("Decide:",i,_kind)
        loop i image sim
      ReadMem a f -> do
        let b = readMem image a
        printf "r: %s --> %s\n" (show a) (show b)
        loop i image (f b)
      WriteMem a b sim -> do
        printf "W: %s <-- %s\n" (show a) (show b)
        loop i (writeMem image (a,b)) sim

data Image = Image { m :: Map Word16 Word8 }

loadImage :: Word16 -> FilePath -> Int  -> IO Image
loadImage offset path expectedSize  = do
  bytes <- loadBytes path
  if length bytes /= expectedSize then error (show ("loadImage",expectedSize,length bytes)) else
    pure Image { m = Map.fromList [ (offset+a,b) | (a,b) <- zip [0..] bytes ] }

readMem :: Image -> Addr -> Byte
readMem Image{m} _a@(Addr w16) = maybe def Byte $ (Map.lookup w16 m)
  where def = Byte 0 -- error (show ("readMem",_a))

writeMem :: Image -> (Addr,Byte) -> Image
writeMem Image{m} (Addr a, Byte b) = Image (Map.insert a b m)


ppState :: Int -> State -> IO ()
ppState i s = do putStrLn (showState i s)

showState :: Int -> State -> String
showState i s = do
  printf
    "halfcycle:%s phi0:%s res:%s AB:%s D:%s RnW:%s PC:%s A:%s X:%s Y:%s SP:%s P:%s IR:%s sync:%s"
    (show i)
    (showBit "clk0")
    (showBit "res")
    (showA "ab")
    (if (lowClock) then "??" else showB "db") -- dont look at dbus when clock is low
    (showBit "rw")
    (showB "pch" ++ showB "pcl")
    (showB "a")
    (showB "x")
    (showB "y")
    (showB "s")
    (showB "p")
    (showB "ir")
    (showBit "sync")
  where
    showBit x = show (Bit (look (ofName x)))
    showB x = show (bitsToByte (map look (ofNameB x)))
    showA x = show (bitsToAddr (map look (ofNameA x)))
    look n = lookState s n

    lowClock = not (look (ofName "clk0"))

ofNameA :: String -> [NodeId]
ofNameA prefix = [ ofName (prefix ++ show i) | i <- reverse [0::Int ..15] ]

ofNameB :: String -> [NodeId]
ofNameB prefix = [ ofName (prefix ++ show i) | i <- reverse [0::Int ..7] ]

----------------------------------------------------------------------
-- copied from Sim2
bitsToAddr :: [Bool] -> Addr -- msb->lsb
bitsToAddr bs =
  if length bs /= 16 then error "bitsToAddr" else
    Addr (foldl (\acc b -> 2*acc+(if b then 1 else 0)) (0::Word16) bs)

bitsToByte :: [Bool] -> Byte -- msb->lsb
bitsToByte bs =
  if length bs /= 8 then error "bitsToByte" else
    Byte (foldl (\acc b -> 2*acc+(if b then 1 else 0)) (0::Word8) bs)
