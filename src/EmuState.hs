
module EmuState
  ( State
  , makeState
  , lookState
  -- , updateState
  , getRW, getAB, getDB
  , Inputs
  , fixedInputs
  , resetLO, resetHI
  , posClk, negClk
  , setInputByte
  , applyInputs
  ) where

import Assigns (NodeId(..))
import Data.Map (Map)
import NodeNames (ofName,toName)
import Values (Byte,Addr,bitsToByte,bitsToAddr,splitB)
import qualified Data.Map as Map

data State = State (Map NodeId Bool) deriving (Eq)

makeState :: [(NodeId,Bool)] -> State
makeState xs = State (Map.fromList xs)

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

applyInputs :: Inputs -> State -> State
applyInputs pairs s =
  foldl updateState s [ (ofName name, b) | (name,b) <- pairs ]

type Inputs = [(String,Bool)]

resetLO :: Inputs
resetLO = [("res",False)]

resetHI :: Inputs
resetHI = [("res",True)]

posClk :: Inputs
posClk = [("clk0",True)]

negClk :: Inputs
negClk = [("clk0",False)]

setInputByte :: String -> Byte -> Inputs
setInputByte prefix byte =
  [ ( (prefix ++ show i), bool)
  | (i,bool) <- zip (reverse [0::Int .. 7]) (splitB byte)
  ]

fixedInputs :: Inputs
fixedInputs =
  [ ("vcc",True)
  , ("vss",False)
  , ("so",True) -- True prevents the Set-Overflow behaviour
  , ("rdy",True)
  , ("nmi",True)
  , ("irq",True)
  ]

ofNameA :: String -> [NodeId]
ofNameA prefix = [ ofName (prefix ++ show i) | i <- reverse [0::Int ..15] ]

ofNameB :: String -> [NodeId]
ofNameB prefix = [ ofName (prefix ++ show i) | i <- reverse [0::Int ..7] ]
