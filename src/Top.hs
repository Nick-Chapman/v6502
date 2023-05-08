
module Top (main) where

import Data.Set (size)
import Logic (Line,AssignDef(..),Exp(..),NodeId)
import NodeNames (toName)
import Norm (normalize)
import ParseLogic (parseLogicLines)
import Pretty (ppLine,ppAssignDef)
import qualified Data.Set as Set

main :: IO ()
main = do
  logic0 <- parseLogicLines <$> readFile "data/logic.inc"
  generateFile "logic0" (LogicRaw logic0)
  let logic1 = normalize logic0
  generateFile "logic1-just-assigns" (Assigns logic1)

  let defs = Set.fromList [ n | AssignDef n _ <- logic1 ]
  let refs = Set.fromList [ n | AssignDef _ e <- logic1, n <- nrefsOfExp e ]

  let rNd = refs `Set.difference` defs
  let dNr = defs `Set.difference` refs

  print ("#assigns",length logic1)
  print ("#defs",size defs)
  print ("#refs",size refs)
  print ("rNd",size rNd, Set.map toName rNd)
  print ("dNr",size dNr, Set.map toName dNr)


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


data LogicRaw = LogicRaw [Line]
instance Show LogicRaw where show (LogicRaw xs) = unlines (map ppLine xs)

data Assigns = Assigns [AssignDef]
instance Show Assigns where show (Assigns as) = unlines (map ppAssignDef as)
