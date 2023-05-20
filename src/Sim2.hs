
module Sim2
  ( main
  , Version(..)
  , Sim(..), theSim
  , State, lookState
  , Addr(..), Byte(..), Bit(..)
  , bitsToAddr
  , ofNameB
  , StateSum(..)
  ) where

import Data.List (intercalate)
--import Data.Char (toUpper)
import Data.Map (Map)
import Data.Set (Set,size,difference,union)
import Data.Word (Word8,Word16)
import Exp (nrefsOfExp,subNode)
import Logic (AssignDef(..),Exp(..),NodeId(..))
import NodeNames (ofName,toName,isNamed)
import Norm (normalize)
import ParseLogic (parseLogicLines)
import Text.Printf (printf)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Misc (hist,the,nub)


main :: Version -> IO ()
main v = do
  sim <- theSim v
  let mem = memHardWiredNop
  run 35 sim mem

theSim :: Version -> IO Sim
theSim v = do
  logic <- getLogic v
  -- print (Summary logic)
  pure (simGivenLogic logic)


data Sim
  = Stabilization (Maybe Int) Sim
  | NewState State Sim
  | Decide Addr CycleKind Sim
  | ReadMem Addr (Byte -> Sim)
  | WriteMem Addr Byte Sim

data CycleKind = ReadCycle | WriteCycle deriving Show

run :: Int -> Sim -> Mem -> IO ()
run n sim mem = loop 0 sim
  where
    loop :: Int -> Sim -> IO ()
    loop i sim = do
      if i == n then print "*stop*" else do
        case sim of
          Stabilization stab sim -> do
            --print (i,"stabilization",stab)
            printf "stabilization in %s\n" (show stab)
            loop i sim
          NewState _state sim -> do
            print (StateSum i _state)
            loop i sim
          Decide addr kind sim -> do
            print (i,"Decide:",addr,kind)
            loop i sim
          ReadMem a f -> do
            let b = readMem mem a
            --print (i,"read-mem",a,"-->",b)
            printf "r: %s --> %s\n" (show a) (show b)
            loop (i+1) (f b)
          WriteMem a b sim -> do
            --print (i,"WRITE-MEM",a,"<--",b)
            printf "W: %s <-- %s\n" (show a) (show b)
            loop (i+1) sim


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

data Version = Raw | Simp | Minimal

instance Show Version where
  show = \case Raw -> "unoptimized"; Simp -> "simplified"; Minimal -> "minimal"

getLogic :: Version -> IO Logic
getLogic v = do
  let conv = case v of Raw -> pure; Simp -> pure . simplify; Minimal -> minimize
  assigns0 <- normalize <$> parseLogicLines <$> readFile "data/logic_unopt.inc"
  assigns <- conv assigns0
  let m = Map.fromList [ (n,e) | AssignDef n e <- assigns ]
  pure Logic{ name = show v, m }

simplify :: [AssignDef] -> [AssignDef]
simplify assigns = do
  let save = Set.fromList (map ofName saveNames)
        where saveNames = ["rw","sync"]
                ++ [ "db"++show @Int n | n <- [0..7] ]
                ++ [ "ab"++show @Int n | n <- [0..15] ]
                ++ [ "ir"++show @Int n | n <- [0..7] ]
                ++ [ "p"++show @Int n | n <- [0..7] ]

  let defined2 = Set.fromList [ n | AssignDef n _ <- assigns ]
  let triv2 = Set.fromList (detectTrivNodes assigns)
  let once2 = Set.fromList (detectUsedOne assigns) `Set.intersection` defined2
  let toElim2 = (triv2 `Set.union` once2) `Set.difference` save
  foldl inlineNodeId assigns toElim2

detectTrivNodes :: [AssignDef] -> [NodeId]
detectTrivNodes as = [ n | AssignDef n e <- as, isTrivRHS e ]

detectUsedOne :: [AssignDef] -> [NodeId]
detectUsedOne as = do
  let used = [ n | AssignDef _ e <- as, n <- nrefsOfExp e ]
  [ n | (n,1) <- Map.toList (hist used) ]

inlineNodeId :: [AssignDef] -> NodeId -> [AssignDef]
inlineNodeId as nToBeInlined = do
  let nBody = the (show nToBeInlined) [ e | AssignDef n e <- as, n == nToBeInlined ]
  let f = subNode (\n -> if n == nToBeInlined then nBody else ENode n)
  [ AssignDef n (f e) | AssignDef n e <- as, n /= nToBeInlined ]

isTrivRHS :: Exp -> Bool
isTrivRHS = \case
  ENode{} -> True
  EWire{} -> True
  EConst{} -> True
  ENot x -> isTrivRHS x
  EAnd{} -> False
  EOr{} -> False
  EXor{} -> False
  EIte{} -> False

----------------------------------------------------------------------
-- minimize logic...

minimize :: [AssignDef] -> IO [AssignDef]
minimize xs = do
  --print "*minimize"

  let defs = Set.fromList [ n | AssignDef n _ <- xs ]
  let refs = Set.fromList [ n | AssignDef _ e <- xs, n <- nrefsOfExp e ]
  let all = defs `union` refs
  --print ("#all", length all)

  let rNd = refs `difference` defs -- inputs
  --print ("#rNd", length rNd)

  let dNr = defs `difference` refs -- outputs
  --print ("#rNd", length dNr)

  let wantNames = []
        ++ [ "db"++show @Int n | n <- [0..7] ]
        ++ [ "ir"++show @Int n | n <- [0..7] ]
        ++ [ "a"++show @Int n | n <- [0..7] ]
        ++ [ "x"++show @Int n | n <- [0..7] ]
        ++ [ "y"++show @Int n | n <- [0..7] ]
        ++ [ "pcl"++show @Int n | n <- [0..7] ]
        ++ [ "pch"++show @Int n | n <- [0..7] ]
        ++ [ "s"++show @Int n | n <- [0..7] ]
        ++ [ "p"++show @Int n | n <- [0..7] ]

  {-
        ++ [ "idb"++show @Int n | n <- [0..7] ]
        ++ [ "adl"++show @Int n | n <- [0..7] ]
        ++ [ "adh"++show @Int n | n <- [0..7] ]
        ++ [ "sb"++show @Int n | n <- [0..7] ]
        ++ [ "abl"++show @Int n | n <- [0..7] ]
        ++ [ "abh"++show @Int n | n <- [0..7] ]
-}
--        ++ [ "idl"++show @Int n | n <- [0..7] ]
--        ++ [ "alua"++show @Int n | n <- [0..7] ]
--        ++ [ "alub"++show @Int n | n <- [0..7] ]
--        ++ [ "alu"++show @Int n | n <- [0..7] ]
        ++ [ "cp1", "cp2", "t3", "t4", "clock1","clock2" ]

  let want = map ofName wantNames
  --print ("#want", length want)

  --let keep = all `difference` Set.fromList [NodeId 209]
  --let keep = all `difference` Set.fromList [NodeId 1693] -- bogus, 1693 needed
  --let keep = Set.fromList (map NodeId [0::Int .. 1700]) -- bogus, losses 1702/s4
  let keep = computeNeed want xs `union` rNd `union` dNr
  --print ("#keep", length keep)

  let toElim = all `difference` keep
  --print ("#toElim", length toElim)
  let _ = print ("toElim", show [ toName n | n <- Set.toList toElim, isNamed n])

  pure (foldl inlineNodeId xs toElim)


computeNeed :: [NodeId] -> [AssignDef] -> Set NodeId
computeNeed want xs = travNs (Set.fromList want) [] defs
  where

    defs = [ n | AssignDef n _ <- xs ]

    m = Map.fromList [ (n,e) | AssignDef n e <- xs ]

    travN :: Set NodeId -> [NodeId] -> NodeId -> Set NodeId
    travN acc chain n = do
      if Set.member n acc then acc else
        if n `elem` chain then Set.insert n acc else do
          case Map.lookup n m of
            Nothing -> acc
            Just exp -> travNs acc (n:chain) (nub (nrefsOfExp exp))

    travNs :: Set NodeId -> [NodeId] -> [NodeId] -> Set NodeId
    travNs acc chain = \case
      [] -> acc
      n:ns -> do
        let acc' = travN acc chain n
        travNs acc' chain ns


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
    let ops = sum [  opCount e | (_,e) <- xs ]
    unlines
      [ name
      , "- #all     = " ++ show (size all)
      , "- #defs    = " ++ show (size defs)
      , "- #refs    = " ++ show (size refs)
      , "- rNd      = " ++ show (map toName rNd)
      , "- dNr      = " ++ show (map toName dNr)
      , "- #OPS     = " ++ show ops
      ]

opCount :: Exp ->  Int
opCount = c
  where
    c = \case
      ENode{} -> 0
      EWire{} -> 0
      ENot x -> 1 + c x
      EAnd x y -> 1 + c x + c y
      EOr x y -> 1 + c x + c y
      EXor x y -> 1 + c x + c y
      EIte x y z -> 1 + c x + c y + c z
      EConst{} -> 0

----------------------------------------------------------------------
-- Bit, Addr, Byte

data Bit = Bit Bool
instance Show Bit where show (Bit bool) = if bool then "1" else "0"

data Addr = Addr Word16
--instance Show Addr where show (Addr w16) = printf "[%04x]" w16
instance Show Addr where
--  show (Addr w16) = map toUpper (printf "%04x" w16)
  show (Addr w16) = printf "%04x" w16

data Byte = Byte Word8
--instance Show Byte where show (Byte w8) = printf "[%02x]" w8
instance Show Byte where
--  show (Byte w8) = map toUpper (printf "%02x" w8)
  show (Byte w8) = printf "%02x" w8


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

----------------------------------------------------------------------

data StateSum = StateSum
  { _stepNum :: Int
  , _nextState :: State
  }

instance Show StateSum where
  show (StateSum i s) = do
    let look n = lookState s n
    intercalate " "
      [ printf " %i" i
      , "rw=" ++ show (Bit (look (ofName "rw")))
      , "sync=" ++ show (Bit (look (ofName "sync")))
      , "c=" ++ show (Bit (look (ofName "clk0")))
      , "r=" ++ show (Bit (look (ofName "res")))
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
