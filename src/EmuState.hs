
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
  , showState
  ) where

import Assigns (NodeId(..))
import Data.Map (Map)
import NodeNames (ofName,toName)
import Values (Bit(..),Byte,Addr,bitsToByte,bitsToAddr,splitB)
import qualified Data.Map as Map
import Text.Printf (printf)

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

showState :: Int -> State -> String
showState i s = do
  let rw = look (ofName "rw")
  let db = showB "db"
  let ab = showA "ab"
  printf
    "halfcycle:%s phi0:%s res:%s AB:%s RnW:%s PC:%s A:%s X:%s Y:%s SP:%s P:%s IR:%s sync:%s%s"
    (show i)
    (showBit "clk0")
    (showBit "res")
    ab
    (show (Bit rw))
    (showB "pch" ++ showB "pcl")
    (showB "a")
    (showB "x")
    (showB "y")
    (showB "s")
    (showB "p")
    (showB "ir")
    (showBit "sync")
    (if lowClock then "" else (if rw then " r:" else " W:") ++ printf "%s=%s" ab db)
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
