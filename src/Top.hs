
module Top (main) where

import Data.Map (Map)
import Data.Set (size)
import Exp (subNode)
import Logic (Line,AssignDef(..),Exp(..),NodeId)
import NodeNames (toName,ofName)
import Norm (normalize)
import ParseLogic (parseLogicLines)
import Pretty (ppLine,ppAssignDef)
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  logic0 <- parseLogicLines <$> readFile "data/logic.inc"
  generateFile "logic0" (LogicRaw logic0)
  let logic1 = normalize logic0
  generateFile "logic1-just-assigns" (Assigns logic1)
  see "1" logic1

  let logic2 = inlineFixedInputs logic1
  generateFile "logic2" (Assigns logic2)
  see "2" logic2

  let outputs = Set.fromList (map ofName outNames)
        where outNames = ["rw","sync"]
                ++ [ "db"++show @Int n | n <- [0..7] ]
                ++ [ "ab"++show @Int n | n <- [0..15] ]

  let triv2 = detectTrivNodes logic2
  let once2 = detectUsedOne logic2
  let toElim2 = Set.toList (Set.fromList (triv2 ++ once2) `Set.difference` outputs)

  --print ("outputs", Set.map toName outputs)
  --print ("triv2", map toName triv2)
  --print ("once2", map toName once2)
  --print ("toElim2", map toName toElim2)

  let logic3 = foldl inlineNodeId logic2 toElim2
  generateFile "logic3" (Assigns logic3)
  see "3" logic3

detectTrivNodes :: [AssignDef] -> [NodeId]
detectTrivNodes as = [ n | AssignDef n e <- as, isTrivRHS e ]

detectUsedOne :: [AssignDef] -> [NodeId]
detectUsedOne as = do
  let used = [ n | AssignDef _ e <- as, n <- nrefsOfExp e ]
  [ n | (n,1) <- Map.toList (hist used) ]


inlineNodeId :: [AssignDef] -> NodeId -> [AssignDef]
inlineNodeId as nToBeInlined = do
  let nBody = the [ e | AssignDef n e <- as, n == nToBeInlined ]
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
  print (tag,"rNd",size rNd, Set.map toName rNd)
  print (tag,"dNr",size dNr, Set.map toName dNr)


generateFile :: Show a => String -> a -> IO ()
generateFile tag a = do
  let fp :: FilePath = "gen/" ++ tag ++ ".out"
  putStrLn $ "Writing file: " <> fp
  writeFile fp (show a)


nrefsOfExp :: Exp -> [NodeId]
nrefsOfExp = loop []
  where
    loop acc = \case
      ENode n -> n : acc
      EWire{} -> acc
      ENot x -> loop acc x
      EAnd x y -> loop (loop acc x) y
      EOr x y -> loop (loop acc x) y
      EXor x y -> loop (loop acc x) y
      EIte x y z -> loop (loop (loop acc x) y) z
      EConst{} -> acc


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

--nub :: Ord a => [a] -> [a]
--nub = Set.toList . Set.fromList

----------------------------------------------------------------------
-- misc

--copied
the :: [a] -> a
the = \case [x] -> x; xs -> error (show ("the",length xs))


hist :: Ord a => [a] -> Map a Int
hist ks = Map.fromList [ (k,length xs) | (k,xs) <- collate [ (k,()) | k <- ks ] ]

collate :: Ord k => [(k,v)] -> [(k,[v])]
collate xs = Map.toList (Map.fromListWith (++) [ (k,[v]) | (k,v) <- xs ])
