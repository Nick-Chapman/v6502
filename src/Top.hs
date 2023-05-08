module Top (main) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Logic (Line(..),AssignDef(..),WireDef(..),MuxDef)
import ParseLogic (parseLogicLines)
import Pretty (ppLine)

main :: IO ()
main = do
  logic0 <- parseLogicRaw <$> readFile "data/logic.inc"
  let logic1 = collateLogic logic0
  generateFile "logic0" logic0
  generateFile "logic1" logic1

generateFile :: Show a => String -> a -> IO ()
generateFile tag a = do
  let fp :: FilePath = "gen/" ++ tag ++ ".out"
  putStrLn $ "Writing file: " <> fp
  writeFile fp (show a)

parseLogicRaw :: String -> LogicRaw
parseLogicRaw = LogicRaw . parseLogicLines

ppLogicRaw :: LogicRaw -> String
ppLogicRaw (LogicRaw xs) = unlines (map ppLine xs)

instance Show LogicRaw where show = ppLogicRaw
instance Show Logic where show = ppLogicRaw . uncollateLogic

data LogicRaw = LogicRaw [Line]

data Logic = Logic
  { assignDefs :: [AssignDef]
  , wireDefs :: [WireDef]
  , muxDefs :: [MuxDef]
  }

uncollateLogic :: Logic -> LogicRaw
uncollateLogic Logic{assignDefs=as,wireDefs=ws,muxDefs=ms} = LogicRaw
  ( []
    ++ map LineA as
    ++ map LineW ws
    ++ map LineM ms
  )

collateLogic :: LogicRaw -> Logic
collateLogic (LogicRaw lines) = Logic
  { assignDefs = sortBy (comparing aLeft) [ x | LineA x <- lines ]
  , wireDefs = sortBy (comparing wLeft) [ x | LineW x <- lines ]
  , muxDefs = [ x | LineM x <- lines ]
  }
