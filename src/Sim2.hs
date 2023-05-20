
module Sim2
  ( Sim(..), simGivenLogic
  , State, lookState
  , Addr(..), Byte(..), Bit(..)
  ) where

import Assigns (Exp(..),NodeId(..))
import Data.Map (Map)
import Data.Word (Word8,Word16)
import GetLogic (Logic(..))
import NodeNames (ofName,toName)
import Text.Printf (printf)
import qualified Data.Map as Map


data Sim
  = Stabilization (Maybe Int) Sim
  | NewState State Sim
  | Decide Addr CycleKind Sim
  | ReadMem Addr (Byte -> Sim)
  | WriteMem Addr Byte Sim

data CycleKind = ReadCycle | WriteCycle deriving Show


-- When clock is high: Addr & R/W line are changed by 6502
-- When clock is low: Data(Byte) is changed, by 6502 (Write), by Mem (Read)

-- So new address (& R/W) appear after negative clock edges
-- And new data-bytes are writen-to or read-from memory on positive clock edges

simGivenLogic :: Logic -> Sim
simGivenLogic logic = do
  let s0 = initState logic
  stabDuringResetPermissive (posClk++extra) s0 $ \s1 -> do
  stabDuringReset (negClk) s1 $ \s2 -> do
  stabDuringReset (posClk) s2 $ \s3 -> do
  stabDuringReset (negClk) s3 $ \s4 -> do
  loop s4

  where
    -- hack to match perfect6502
    extra =
      []
      ++ _setSP (Byte 0xC0)
      ++ _setX (Byte 0xC0)

    loop :: State -> Sim
    loop s0 = do
      -- We collect the r/w line and address bus after a neg clock edge
      let addr = getAB s0
      let kind = if (getRW s0) then ReadCycle else WriteCycle
      Decide addr kind $ do
      case kind of

        ReadCycle -> do
          -- For a Read-Cycle, we present the byte read from memory
          -- on the following negative clock edge.
          -- To get the Dormann trace to look sensible, it seems we also
          -- need to present it earlier on the pos-edge.
          ReadMem addr $ \byte -> do
          stab (posClk ++ setDB byte) s0 $ \s1 -> do -- WHY
          stab (negClk ++ setDB byte) s1 $ \s2 -> do
          loop s2

        WriteCycle -> do
          -- For a Write-Cycle, collect the data to be written to memory
          -- before the next negative lock edge
          --WriteMem addr (getDB s0) $ do -- NO
          stab posClk s0 $ \s1 -> do
          WriteMem addr (getDB s1) $ do
          stab negClk s1 $ \s2 -> do
          loop s2

    stab                      = stabG Strict True
    stabDuringReset           = stabG Strict False
    stabDuringResetPermissive = stabG Permissive False

    stabG :: StabMode -> Bool -> Inputs -> State -> (State -> Sim) -> Sim
    stabG mode res i s k = do
      let (iopt,s') = stabilize mode logic (i ++ [("res",res)] ++ fixedInputs) s
      Stabilization iopt $ do
      NewState s' $ do
      k s'


----------------------------------------------------------------------
-- Bit, Addr, Byte

data Bit = Bit Bool
instance Show Bit where show (Bit bool) = if bool then "1" else "0"

data Addr = Addr Word16
instance Show Addr where show (Addr w16) = printf "%04x" w16

data Byte = Byte Word8
instance Show Byte where show (Byte w8) = printf "%02x" w8

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

_setSP :: Byte -> Inputs
_setSP byte =
  [ ( ("s"++show i), bool)
  | (i,bool) <-zip (reverse [0::Int .. 7]) (splitB byte)
  ]

_setX :: Byte -> Inputs
_setX byte =
  [ ( ("x"++show i), bool)
  | (i,bool) <-zip (reverse [0::Int .. 7]) (splitB byte)
  ]

_setP :: Byte -> Inputs
_setP byte =
  [ ( ("p"++show i), bool)
  | (i,bool) <-zip (reverse [0::Int .. 7]) (splitB byte)
  ]


fixedInputs :: Inputs
fixedInputs =
  [ ("vcc",True)
  , ("vss",False)
  , ("so",True) -- True prevents the Set-VOverflow behaviour
  , ("rdy",True)
  , ("nmi",True)
  , ("irq",True)
  ]


ofNameA :: String -> [NodeId]
ofNameA prefix = [ ofName (prefix ++ show i) | i <- reverse [0::Int ..15] ]

ofNameB :: String -> [NodeId]
ofNameB prefix = [ ofName (prefix ++ show i) | i <- reverse [0::Int ..7] ]



data StabMode = Strict | Permissive

stabilize :: StabMode -> Logic -> Inputs -> State -> (Maybe Int,State)
stabilize mode logic inputs s0 = loop 0 (applyInputs inputs s0)
  where
    max = 50
    err = error (show ("failed to stabilize in",max))
    loop :: Int -> State -> (Maybe Int, State)
    loop i s1 = do
      let s2 = oneStep logic inputs s1
      if s1 == s2 then (Just i, s1) else
        if i == max then (case mode of Strict -> err; Permissive -> (Nothing,s1)) else
          loop (i+1) s2


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
