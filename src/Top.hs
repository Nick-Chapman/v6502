
module Top (main) where

import Logic (Line,AssignDef)
import Norm (normalize)
import ParseLogic (parseLogicLines)
import Pretty (ppLine,ppAssignDef)

main :: IO ()
main = do
  logic0 <- parseLogicLines <$> readFile "data/logic.inc"
  generateFile "logic0" (LogicRaw logic0)
  let logic1 = normalize logic0
  generateFile "logic1-just-assigns" (Assigns logic1)


generateFile :: Show a => String -> a -> IO ()
generateFile tag a = do
  let fp :: FilePath = "gen/" ++ tag ++ ".out"
  putStrLn $ "Writing file: " <> fp
  writeFile fp (show a)


data LogicRaw = LogicRaw [Line]
instance Show LogicRaw where show (LogicRaw xs) = unlines (map ppLine xs)

data Assigns = Assigns [AssignDef]
instance Show Assigns where show (Assigns as) = unlines (map ppAssignDef as)
