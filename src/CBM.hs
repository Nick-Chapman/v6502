
module CBM (main) where

import Data.Bits
import Data.ByteString.Internal (w2c)
import Data.Map (Map)
import EmuState (Sim(..),State,showState,getClock,getPC,getP,getA,getX,getY)
import System.IO (hFlush,stdout)
import Text.Printf (printf)
import Values (Bit(..),Addr,Byte(..),loadBytes)
import qualified Data.Map as Map

main :: Sim -> Int -> Bool -> IO ()
main sim max trace = do
  let path = "../perfect6502/rom/cbmbasic.bin"
  let expectedSize = 17591
  image0 <- loadImage 0xA000 path expectedSize
  let image = setupMonitor image0
  simWithImage max trace image sim
  pure ()

simWithImage :: Int -> Bool -> Image -> Sim -> IO ()
simWithImage max trace = loop 0
  where
    loop :: Int -> Image -> Sim -> IO ()
    loop i image sim = do
     case sim of
      Stabilization _iopt sim -> do
        --printf "stabilization in %s\n" (show _iopt)
        loop i image sim
      NewState state sim -> do
        let _ = if (i `mod` 100 == 0) then do printf " [%d]" i; hFlush stdout else pure ()
        if (trace) then putStrLn (showState i state) else pure ()
        image' <- handleMonitor state image
        if (i==max) then pure () else loop (i+1) image' sim
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


-- comment copied from perfect6502
{-
 * cbmbasic scribbles over 0x01FE/0x1FF, so we can't start
 * with a stackpointer of 0 (which seems to be the state
 * after a RESET), so RESET jumps to 0xF000, which contains
 * a JSR to the actual start of cbmbasic
 -}
setupMonitor :: Image -> Image
setupMonitor image0 =
  foldl writeMem image0 $
  [
  -- reset vector
    (0xfffc, 0x00)
  , (0xfffd, 0xf0)

  -- shim to jump to real entry point
  , (0xf000, 0x20) -- JMP 0xE394
  , (0xf001, 0x94)
  , (0xf002, 0xE3)

  -- stub which kernel functions indirect to
  , (0xf800, 0xA9) -- LDA #P
--, (0xf801, P)
  , (0xf802, 0x48) -- PHA
  , (0xf803, 0xA9) -- LHA #A
--, (0xf804, A)
  , (0xf805, 0xA2) -- LDX #X
--, (0xf806, X)
  , (0xf807, 0xA0) -- LDY #Y
--, (0xf808, Y)
  , (0xf809, 0x28) -- PLP
  , (0xf80a, 0x60) -- RTS
  ] ++
  concat [ [ (k,   0x4C) -- JMP 0xF800
           , (k+1, 0x00)
           , (k+2, 0xF8)
           ]
         | (k,_) <- kernalTable]

handleMonitor :: State -> Image -> IO Image
handleMonitor s i = if not (getClock s) then pure i else do
  let pc = getPC s
  case Map.lookup pc m of
    Just f -> do
      regs' <- f regs
      pure $ setRegsInImage regs' i
    Nothing ->
      case pc of
        0x0001 -> do
          putStrLn "\n\CRASH--PC=1\n"
          pure i -- error "handleMonitor/PC=1"
        _ ->
          pure i
  where
    m = Map.fromList kernalTable
    regs = regsOfState s

data Regs = Regs
  { p :: Byte
  , a :: Byte
  , x :: Byte
  , y :: Byte
  , flags :: Flags
  }

data Flags = Flags
  { n :: Bit
  , z :: Bit
  , c :: Bit
  }

getStatusFlags :: Byte -> Flags
getStatusFlags p = Flags
  { n = Bit (p `testBit` 7)
  , z = Bit (p `testBit` 1)
  , c = Bit (p `testBit` 0)
  }

setStatusFlags :: Byte -> Flags -> Byte
setStatusFlags p Flags{n=Bit n,z=Bit z,c=Bit c} =
  ((flip (if n then setBit else clearBit) 7)
  . (flip (if z then setBit else clearBit) 1)
  . (flip (if c then setBit else clearBit) 0)
  ) p

regsOfState :: State -> Regs
regsOfState s = do
  let p = getP s
  let a = getA s
  let x = getX s
  let y = getY s
  let flags = getStatusFlags  p
  Regs {p, a, x, y, flags}

setRegsInImage :: Regs -> Image -> Image
setRegsInImage Regs{p,a,x,y,flags} i =
  foldl writeMem i
  [ (0xf801, setStatusFlags p flags)
  , (0xf804, a)
  , (0xf806, x)
  , (0xf808, y)
  ]

kernalTable :: [(Addr, Regs -> IO Regs)]
kernalTable =
  [ (0xff99, memtop)
  , (0xff9c, membot)
  , (0xffcf, chrin)
  , (0xffd2, chrout)
  , (0xffe7, pure) -- clall
  , (0xFF90, \r -> pure $ r { a = 0x00 }) -- setmsg
  ]
  where
    membot = \r -> pure $ r { y = 0x08, x = 0x00 } -- membot
    memtop = \r -> pure $ r { y = 0xA0, x = 0x00 } -- memtop


chrout :: Regs -> IO Regs
chrout r@Regs{a,flags} = do
  putStr (charOfByte a); hFlush stdout
  pure $ r { flags = flags { c = Bit False } }

charOfByte :: Byte -> String
charOfByte (Byte w8) = case w8 of
  147 -> "" -- ignore clear screen
  10 -> ""
  13 -> [w2c 13, '\n']
  _ -> [w2c w8]

chrin :: Regs -> IO Regs
chrin = pure  -- TODO
