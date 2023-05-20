
module GetLogic
  ( Version(..)
  , Logic(..)
  , getLogic
  ) where

import Assigns (AssignDef(..),Exp(..),NodeId(..))
import Data.Map (Map)
import Data.Set (Set,size,difference,union)
import Exp (nrefsOfExp,subNode)
import Misc (hist,the,nub)
import NodeNames (ofName,toName,isNamed)
import Norm (normalize)
import ParseLogic (parseLogicLines)
import qualified Data.Map as Map
import qualified Data.Set as Set

data Version = Raw | Simp | Minimal

instance Show Version where
  show = \case Raw -> "unoptimized"; Simp -> "simplified"; Minimal -> "minimal"


data Logic = Logic { name :: String, m :: Map NodeId Exp }

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
