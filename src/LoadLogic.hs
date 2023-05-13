
module LoadLogic (getLogic) where

import Data.Set (size)
import Exp (nrefsOfExp,subNode)
import Logic (Logic(..),Line,AssignDef(..),Exp(..),NodeId)
import Misc (the,hist)
import NodeNames (toNumName,ofName)
import Norm (normalize)
import ParseLogic (parseLogicLines)
import Pretty (ppLine,ppAssignDef)
import qualified Data.Map as Map
import qualified Data.Set as Set

getLogic :: IO Logic
getLogic = do
  --assigns0 <- parseLogicLines <$> readFile "data/logic.inc"
  assigns0 <- parseLogicLines <$> readFile "data/logic_unopt.inc"
  generateFile "logic0" (LogicRaw assigns0)
  let assigns1 = normalize assigns0
  generateFile "logic1-just-assigns" (Assigns assigns1)
  --see "1" assigns1

  let assigns2 = inlineFixedInputs assigns1
  generateFile "logic2" (Assigns assigns2)
  --see "2" assigns2

  let outputs = Set.fromList (map ofName outNames)
        where outNames = ["rw","sync"]
                ++ [ "db"++show @Int n | n <- [0..7] ]
                ++ [ "ab"++show @Int n | n <- [0..15] ]

  let defined2 = Set.fromList [ n | AssignDef n _ <- assigns2 ]
  let triv2 = Set.fromList (detectTrivNodes assigns2)
  let once2 = Set.fromList (detectUsedOne assigns2) `Set.intersection` defined2
  let toElim2 = (triv2 `Set.union` once2) `Set.difference` outputs

  --print ("outputs", Set.map toNumName outputs)
  --print ("#triv2", length triv2)
  --print ("#once2", length once2)
  --print ("#toElim2", length toElim2)

  let assigns3 = foldl inlineNodeId assigns2 toElim2
  generateFile "logic3" (Assigns assigns3)
  --see "3" assigns3

  let assigns = assigns2 -- or 3 -- choose here
  see "logic" assigns
  pure Logic { name = "logic2", assigns }

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

see :: String -> [AssignDef] -> IO ()
see tag logic = do
  let defs = Set.fromList [ n | AssignDef n _ <- logic ]
  let refs = Set.fromList [ n | AssignDef _ e <- logic, n <- nrefsOfExp e ]
  let rNd = refs `Set.difference` defs
  let dNr = defs `Set.difference` refs
  print (tag,"#assigns",length logic)
  print (tag,"#defs",size defs)
  print (tag,"#refs",size refs)
  print (tag,"rNd",size rNd, Set.map toNumName rNd)
  print (tag,"dNr",size dNr, Set.map toNumName dNr)

generateFile :: Show a => String -> a -> IO ()
generateFile tag a = do
  let fp :: FilePath = "gen/" ++ tag ++ ".out"
  putStrLn $ "Writing file: " <> fp
  writeFile fp (show a)

data LogicRaw = LogicRaw [Line]
instance Show LogicRaw where show (LogicRaw xs) = unlines (map ppLine xs)

data Assigns = Assigns [AssignDef]
instance Show Assigns where show (Assigns as) = unlines (map ppAssignDef as)

inlineFixedInputs :: [AssignDef] -> [AssignDef]
inlineFixedInputs as = do
  foldl inlineFixedInput as
    [ ("vcc",True)
    , ("vss",False)
    , ("so",False)
    , ("rdy",True)
    , ("nmi",True)
    , ("irq",True)
    ]

inlineFixedInput :: [AssignDef] -> (String,Bool) -> [AssignDef]
inlineFixedInput as (name,value) = do
  let nToBeInlined = ofName name
  let f = subNode (\n -> if n == nToBeInlined then EConst value else ENode n)
  [ AssignDef n (f e) | AssignDef n e <- as ]

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
