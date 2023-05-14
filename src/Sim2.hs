
module Sim2(main) where

import Data.List (intercalate)
import Data.Map (Map)
import Data.Set (size,difference,union)
import Data.Word (Word8,Word16)
import Exp (nrefsOfExp)
import Logic (AssignDef(..),Exp(..),NodeId)
import NodeNames (ofName,toName)
import Norm (normalize)
import ParseLogic (parseLogicLines)
import Text.Printf (printf)
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  logic <- getLogic
  print (Summary logic)
  let mem = memHardWiredNop
  run 35 logic mem

run :: Int -> Logic -> Mem -> IO ()
run n logic mem = loop state0 0 (sim logic)
  where
    state0 = initState logic
    loop :: State -> Int -> Sim -> IO ()
    loop _s0 i sim = do
      if i == n then print "*stop*" else do
        case sim of
          ReadCycle s1 a f -> do
            print (StateSum i s1)
            let b = readMem mem a
            print ("read",a,"-->",b)
            let (s2,sim) = f b
            print (StateSum i s2)
            loop s2 (i+1) sim
          WriteCycle s1 a s2 b sim -> do
            print (StateSum i s1)
            print ("WRITE",a,"<--",b)
            print (StateSum i s2)
            loop s2 (i+1) sim


sim :: Logic -> Sim
sim logic = loop (reset logic)
  where
    res1 = [("res",True)]
    loop :: State -> Sim
    loop s0 = do
      let addr = getAB s0
      if (getRW s0)
        then
        do
          ReadCycle s0 addr $ \byte -> do
            let s1 = stabilize logic (setDB byte ++ posClk ++ res1 ++ fixedInputs) s0
            let s2 = stabilize logic (negClk ++ res1 ++ fixedInputs) s1
            (s1, loop s2)
        else
        do
          let s1 = stabilize logic (posClk ++ res1 ++ fixedInputs) s0
          let byte = getDB s1
          let s2 = stabilize logic (negClk ++ res1 ++ fixedInputs) s1
          WriteCycle s0 addr s1 byte (loop s2)


----------------------------------------------------------------------

-- When clock is high: Addr & R/W line are changed by 6502
-- When clock is low: Data(Byte) is changed, by 6502 (Write), by Mem (Read)

-- So new address (& R/W) appear after negative clock edges
-- And new data-bytes are writen-to or read-from memory on positive clock edges

data Sim
  = ReadCycle State Addr (Byte -> (State,Sim))
  | WriteCycle State Addr State Byte Sim

reset :: Logic -> State
reset logic = do
  let res0 = [("res",False)]
  let res1 = [("res",True)]
  let s0 = initState logic
  let s1 = stabilizeLIB logic (posClk ++ res0 ++ fixedInputs) s0
  let s2 = stabilize logic (negClk ++ res0 ++ fixedInputs) s1
  let s3 = stabilize logic (posClk ++ res0 ++ fixedInputs) s2
  let s4 = stabilize logic (negClk ++ res0 ++ fixedInputs) s3
  let s5 = stabilize logic (negClk ++ res1 ++ fixedInputs) s4
  s5

----------------------------------------------------------------------

data Mem = MEM

memHardWiredNop :: Mem
memHardWiredNop = MEM

--readMem :: Mem -> Addr -> Byte
--readMem _ _ = Byte 0xEA

readMem :: Mem -> Addr -> Byte
readMem _ = Byte . \case
  Addr 0xfffc -> 0x22
  Addr 0xfffd -> 0x11
  Addr 0x1122 -> 0xA2 -- LDX #
  Addr 0x1123 -> 0x77
  Addr 0x1124 -> 0x8E -- STX abs
  Addr 0x1125 -> 0x44
  Addr 0x1126 -> 0x33
  Addr 0x1127 -> 0xE8 -- INX

  Addr 0x1128 -> 0x4C -- JMP abs
  Addr 0x1129 -> 0x24
  Addr 0x112A -> 0x11

  _ -> 0xEA


----------------------------------------------------------------------
data Logic = Logic { name :: String, m :: Map NodeId Exp }

----------------------------------------------------------------------

getLogic :: IO Logic
getLogic = do
  assigns <- normalize <$> parseLogicLines <$> readFile "data/logic_unopt.inc"
  let m = Map.fromList [ (n,e) | AssignDef n e <- assigns ]
  pure Logic{ name = "unoptimized", m }

----------------------------------------------------------------------
data Summary = Summary Logic

instance Show Summary where
  show (Summary (Logic{name,m})) = do
    let xs = Map.toList m
    let defs = Set.fromList [ n | (n,_) <- xs ]
    let refs = Set.fromList [ n | (_,e) <- xs, n <- nrefsOfExp e ]
    let all = defs `union` refs
    let rNd = Set.toList (refs `difference` defs)
    let dNr = Set.toList (defs `difference` refs)
    unlines
      [ name
      , "- #all     = " ++ show (size all)
      , "- #defs    = " ++ show (size defs)
      , "- #refs    = " ++ show (size refs)
      , "- rNd      = " ++ show (map toName rNd)
      , "- dNr      = " ++ show (map toName dNr)
      ]

----------------------------------------------------------------------
-- Bit, Addr, Byte

data Bit = Bit Bool
instance Show Bit where show (Bit bool) = if bool then "1" else "0"

data Addr = Addr Word16
instance Show Addr where show (Addr w16) = printf "[%04x]" w16

data Byte = Byte Word8
instance Show Byte where show (Byte w8) = printf "[%02x]" w8


bitsToAddr :: [Bool] -> Addr -- msb->lsb
bitsToAddr bs =
  if length bs /= 16 then error "bitsToAddr" else
    Addr (foldl (\acc b -> 2*acc+(if b then 1 else 0)) (0::Word16) bs)

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

----------------------------------------------------------------------
data State = State (Map NodeId Bool) deriving (Eq)

initState :: Logic -> State
initState (Logic{m}) = do
  State (Map.fromList [ (n,False) | (n,_) <- Map.toList m ])


updateState :: State -> (NodeId,Bool) -> State
updateState (State m) (n,v) = State (Map.insert n v m)

getAB :: State -> Addr
getAB s = bitsToAddr (map (lookState s) (ofNameA "ab"))

getDB :: State -> Byte
getDB s = bitsToByte (map (lookState s) (ofNameB "db"))

getRW :: State -> Bool -- 1:read, 0:wrrite
getRW s = lookState s (ofName "rw")

lookState :: State -> NodeId -> Bool
lookState (State m) n = maybe err id $ Map.lookup n m
  where err = error (show ("lookState",n,toName n))

----------------------------------------------------------------------

type Inputs = [(String,Bool)]

applyInputs :: Inputs -> State -> State
applyInputs pairs s =
  foldl updateState s [ (ofName name, b) | (name,b) <- pairs ]

posClk :: Inputs
posClk = [("clk0",True)]

negClk :: Inputs
negClk = [("clk0",False)]

setDB :: Byte -> Inputs
setDB byte =
  [ ( ("db"++show i), bool)
  | (i,bool) <-zip (reverse [0::Int .. 7]) (splitB byte)
  ]

fixedInputs :: Inputs
fixedInputs =
  [ ("vcc",True)
  , ("vss",False)
  , ("so",False)
  , ("rdy",True)
  , ("nmi",True)
  , ("irq",True)
  ]

----------------------------------------------------------------------

data StateSum = StateSum
  { _stepNum :: Int
  , _nextState :: State
  }

instance Show StateSum where
  show (StateSum i s) = do
    let look n = lookState s n
    intercalate " "
      [ "i=" ++ printf "%3i" i
      , "rw=" ++ show (Bit (look (ofName "rw")))
      , "sync=" ++ show (Bit (look (ofName "sync")))
      , "c=" ++ show (Bit (look (ofName "clk0")))
      , "IR=" ++ show (bitsToByte (map look (ofNameB "ir")))
      , "PC=" ++ show (bitsToAddr (map look (ofNameB "pch" ++ ofNameB "pcl")))
--      , "SP=" ++ show (bitsToByte (map look (ofNameB "s")))
      , "A=" ++ show (bitsToByte (map look (ofNameB "a")))
      , "X=" ++ show (bitsToByte (map look (ofNameB "x")))
--      , "Y=" ++ show (bitsToByte (map look (ofNameB "y")))
      , "ab=" ++ show (bitsToAddr (map look (ofNameA "ab")))
      , "db=" ++ show (bitsToByte (map look (ofNameB "db")))
      ]

ofNameA :: String -> [NodeId]
ofNameA prefix = [ ofName (prefix ++ show i) | i <- reverse [0::Int ..15] ]

ofNameB :: String -> [NodeId]
ofNameB prefix = [ ofName (prefix ++ show i) | i <- reverse [0::Int ..7] ]


stabilize :: Logic -> Inputs -> State -> State
stabilize logic inputs s0 = loop max (applyInputs inputs s0)
  where
    max = 50
    err = error (show ("failed to stabilize in",max))
    loop :: Int -> State -> State
    loop i s1 = do
      let s2 = oneStep  logic inputs s1
      if s1 == s2 then s1 else
        if i == 0 then err else
          loop (i-1) s2

stabilizeLIB :: Logic -> Inputs -> State -> State
stabilizeLIB logic inputs s0 = loop max (applyInputs inputs s0)
  where
    max = 50
    loop :: Int -> State -> State
    loop i s1 = do
      let s2 = oneStep logic inputs s1
      if s1 == s2 then s1 else
        if i == 0 then s1 else
          loop (i-1) s2


oneStep :: Logic -> Inputs -> State -> State
oneStep Logic{m} inputs s0 = applyInputs inputs s'
  where
    s' = State (Map.fromList [ (n,eval e) | (n,e) <- Map.toList m ])

    eval :: Exp -> Bool
    eval = \case
      ENode n -> lookState s0 n
      EWire{} -> error "eval/wire"
      EConst b -> b
      ENot x -> not (eval x)
      EAnd x y -> eval x && eval y
      EOr x y -> eval x || eval y
      EXor x y -> eval x /= eval y
      EIte x y z -> eval (if eval x then y else z)
