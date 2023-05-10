
module Sim (main) where

import Data.List (sort,intercalate)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import Data.Set (Set)
import Data.Word (Word8,Word16)
import Exp (nrefsOfAssigns)
import Logic (AssignDef(..),Exp(..),NodeId)
import NodeNames (toName,ofName)
import Text.Printf (printf)
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: [AssignDef] -> IO ()
main theAssigns = do
  print ("sim, #assigns=",length theAssigns)
  let keys = initKeys theAssigns
  print ("#keys",let (Keys set) = keys in Set.size set)
  let shape = makeShape keys
  let
    simulate :: Int -> State -> [Inputs] -> IO ()
    simulate n s1 = \case
      [] -> print "*finish*"
      inputs:more -> do
        s2 <- viewSim n theAssigns shape inputs s1
        simulate (n+1) s2 more
  simulate 1 (initState keys) makeInputs

----------------------------------------------------------------------
-- Inputs

data Inputs = Inputs [(String,Bool)]
  deriving (Show)

setInputs :: Inputs -> State -> State
setInputs (Inputs pairs) s0 =
  foldl updateState s0 [ (ofName name,v) | (name,v) <- pairs ]

updateState :: State -> (NodeId,Bool) -> State
updateState (State m) (n,v) = State (Map.insert n v m)

makeInputs :: [Inputs]
makeInputs = do
  reset False (clockCycles 3) ++ reset True (clockCycles 12)
  where
    clockCycles :: Int -> [Inputs]
    clockCycles n = take (2*n) clock
      where
        clock = [Inputs [("clk0",True)],Inputs[("clk0",False)]] ++ clock

    reset :: Bool -> [Inputs] -> [Inputs]
    reset bool xss = [ Inputs (("res",bool) : xs) | Inputs xs <- xss ]

-- set db to be fixed NOP (0xEA) xpattern
fixDB :: Inputs -> Inputs
fixDB (Inputs xs) = Inputs (extra ++ xs)
  where extra = [ ("db"++show i, bool)
                | (i,bool) <-zip (reverse [0::Int .. 7]) (splitB (Byte 0xEA)) ]

----------------------------------------------------------------------
-- Keys

data Keys = Keys (Set NodeId)

initKeys :: [AssignDef] -> Keys
initKeys assigns = Keys (nrefsOfAssigns assigns)

----------------------------------------------------------------------
-- Shape

data Shape = Shape [[NodeId]]

makeShape :: Keys -> Shape
makeShape (Keys set) = do
  let xs = map snd $ sort [ (toName n, n) | n <- Set.toList set ]
  --let xs = map fst $ sort [ (n, toName n) | n <- Set.toList set ]
  Shape (chunksOf 70 xs)

----------------------------------------------------------------------
-- State
data State = State (Map NodeId Bool)

initState :: Keys -> State -- all zero
initState (Keys set) =
  State (Map.fromList [ (n,False) |  n <- Set.toList set ])

----------------------------------------------------------------------
-- Change

data Change = Change (Map NodeId Edge)
data Edge = Low | Pos | High | Neg -- deriving (Eq,Ord,Show)

compChange :: State -> State -> Change
compChange (State m1) (State m2) = Change (Map.map compEdge (zipMap m1 m2))

compEdge :: (Bool,Bool) -> Edge
compEdge = \case
  (False,False) -> Low
  (False,True ) -> Pos
  (True, True ) -> High
  (True, False) -> Neg

isStable :: Change -> Bool
isStable x = numDiffs x == 0 -- TODO: better to not count

numDiffs :: Change -> Int
numDiffs (Change m) = do
  length [ () | (_,e) <- Map.toList m, isChangeEdge e ]
    where isChangeEdge = \case Pos -> True; Neg -> True; High -> False; Low -> False

changeSum :: Change -> (Int,Int)
changeSum (Change m) = (count isPos, count isNeg)
  where
    count pred = length [ () | (_,e) <- Map.toList m, pred e ]
    isPos = \case Pos -> True; _ -> False
    isNeg = \case Neg -> True; _ -> False

----------------------------------------------------------------------
-- viewSim, stabilize

viewSim :: Int -> [AssignDef] -> Shape -> Inputs -> State -> IO State
viewSim i assigns shape inputs0 s0 = do
  let _ = see
  --print (i,inputs0)
  let inputs = fixDB inputs0 -- why add "db" here?
  let res = stabilize assigns inputs (setInputs inputs s0)
  case res of
    Unstable _changes (s1,_) -> do
      --see _changes
      print (HighLevelStateView i Nothing s1)
      pure s1
    Stable changes s1 -> do
      --see changes
      print (HighLevelStateView i (Just (length changes)) s1)
      pure s1
  where
    see :: [Change] -> IO ()
    see changes = do
      mapM_ pr (zip [1::Int ..] changes)
      print ("changeSum=",map changeSum changes)

    pr :: (Int,Change) -> IO ()
    pr (j,c) = do
      printf "%d.%d\n" i j
      print (ChangeView shape c)

data StabRes
  = Stable [Change] State
  | Unstable [Change] (State,State)

stabilize :: [AssignDef] -> Inputs -> State -> StabRes
stabilize assigned inputs = loop 20 []
  where
    loop :: Int -> [Change] -> State -> StabRes
    loop i acc s1 = do
      let s2 = step inputs i assigned s1
      let c = compChange s1 s2
      let acc' = c : acc
      if isStable c then Stable (reverse acc') s2 else
        if i == 0 then Unstable (reverse acc') (s1,s2) else
          loop (i-1) acc' s2

----------------------------------------------------------------------
-- (evaluation) step

step :: Inputs -> Int -> [AssignDef] -> State -> State
step inputs tag assigns (State m0) = setInputs inputs s'
  where
    s' = State (Map.fromList [ (n,eval e) | AssignDef n e <- assigns ])

    lookup :: NodeId -> Bool
    lookup nid = maybe err id $ Map.lookup nid m0
      where err = error (show ("lookup",tag,nid,toName nid))

    eval :: Exp -> Bool
    eval = \case
      ENode n -> lookup n
      EWire{} -> error "eval/wire"
      EConst b -> b
      ENot x -> not (eval x)
      EAnd x y -> eval x && eval y
      EOr x y -> eval x || eval y
      EXor x y -> eval x /= eval y
      EIte x y z -> eval (if eval x then y else z)

----------------------------------------------------------------------
-- ChangeView

data ChangeView = ChangeView Shape Change

instance Show ChangeView where
  show (ChangeView (Shape nss) (Change m)) = do
    let look n = maybe err id $ Map.lookup n m
          where err = error (show ("ChangeView",n))
    let pp = \case Low -> '.'; Pos -> '/'; High -> '#'; Neg -> '\\'
    unlines [ "  " ++ [ pp (look n) | n <- ns ] | ns <- nss ]

----------------------------------------------------------------------
-- HighLevelStateView

data HighLevelStateView = HighLevelStateView
  { _stepNum :: Int
  , _stepsToStabilize :: Maybe Int -- Nothing means didn't stabilize
  , _nextState :: State
  }

instance Show HighLevelStateView where
  show (HighLevelStateView i q (State m)) = do
    let look n = maybe err id $ Map.lookup n m
          where err = error (show ("HighLevelStateView",n))
    let stab =
          case q of
            Nothing -> "*UNSTABLE*"
            Just q -> printf "{stab: %2i}" q
            --Just{} -> "STAB"
    intercalate " "
      [ "i=" ++ printf "%3i" i
      , stab
      , "r=" ++ show (Bit (look (ofName "res")))
      , "c=" ++ show (Bit (look (ofName "clk0")))

--    , "IR=" ++ show (bitsToByte (map look (ofNameB "ir"))) -- bogus nids

      , "PC=" ++ show (bitsToAddr (map look (ofNameB "pch" ++ ofNameB "pcl")))
      , "SP=" ++ show (bitsToByte (map look (ofNameB "s")))
      , "A=" ++ show (bitsToByte (map look (ofNameB "a")))
      , "X=" ++ show (bitsToByte (map look (ofNameB "x")))
      , "Y=" ++ show (bitsToByte (map look (ofNameB "y")))

      , "ab=" ++ show (bitsToAddr (map look (ofNameA "ab")))
      , "rw=" ++ show (Bit (look (ofName "rw")))
      , "db=" ++ show (bitsToByte (map look (ofNameB "db")))
      ]

ofNameA :: String -> [NodeId]
ofNameA prefix = [ ofName (prefix ++ show i) | i <- reverse [0::Int ..15] ]

ofNameB :: String -> [NodeId]
ofNameB prefix = [ ofName (prefix ++ show i) | i <- reverse [0::Int ..7] ]

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
-- misc

zipMap :: Ord k => Map k v1 -> Map k v2 -> Map k (v1,v2)
zipMap = Map.intersectionWith (\a b -> (a,b))
