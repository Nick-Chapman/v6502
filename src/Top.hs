module Top (main) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Logic (Line(..),AssignDef(..),WireDef(..),MuxDef(..),WireId,Exp(..),Vec(..))
import ParseLogic (parseLogicLines)
import Pretty (ppLine)

import qualified Data.Map as Map
import Data.Map (Map)

main :: IO ()
main = do
  logic0 <- parseLogicRaw <$> readFile "data/logic.inc"
  let logic1 = collateLogic logic0
  generateFile "logic0" logic0
  generateFile "logic1" logic1
  let wu = wireUsage logic1
  mapM_ print (Map.toList wu)

-- check wireId refs...

type WireUsage = Map WireId Int

wireUsage :: Logic -> WireUsage
wireUsage = hist . wrefsOfLogic

wrefsOfLogic :: Logic -> [WireId]
wrefsOfLogic logic = expsOfLogic logic >>= wrefsOfExp

wrefsOfExp :: Exp -> [WireId]
wrefsOfExp = loop []
  where
    loop acc = \case
      ENode{} -> []
      EWire w -> w : acc
      ENot x -> loop acc x
      EAnd x y -> loop (loop acc x) y
      EOr x y -> loop (loop acc x) y
      EXor x y -> loop (loop acc x) y
      EIte x y z -> loop (loop (loop acc x) y) z

----------------------------------------------------------------------
-- Logic

instance Show Logic where show = ppLogicRaw . uncollateLogic

data Logic = Logic
  { assignDefs :: [AssignDef]
  , wireDefs :: [WireDef]
  , muxDefs :: [MuxDef]
  }

expsOfLogic :: Logic -> [Exp]
expsOfLogic Logic{assignDefs,wireDefs,muxDefs} =
  [ e | AssignDef{aRight=e} <- assignDefs ] ++
  [ e | WireDef{wRight=e} <- wireDefs ] ++
  [ e | m <- muxDefs, e <- expsOfMuxDef m ]

expsOfMuxDef :: MuxDef -> [Exp]
expsOfMuxDef MuxDef{i,s,d} = [i] ++ unVec s ++ unVec d

----------------------------------------------------------------------
-- (un)collate

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

----------------------------------------------------------------------
-- LogicRaw

data LogicRaw = LogicRaw [Line]

instance Show LogicRaw where show = ppLogicRaw

ppLogicRaw :: LogicRaw -> String
ppLogicRaw (LogicRaw xs) = unlines (map ppLine xs)

parseLogicRaw :: String -> LogicRaw
parseLogicRaw = LogicRaw . parseLogicLines

----------------------------------------------------------------------
-- misc

hist :: Ord a => [a] -> Map a Int
hist ks = Map.fromList [ (k,length xs) | (k,xs) <- collate [ (k,()) | k <- ks ] ]

collate :: Ord k => [(k,v)] -> [(k,[v])]
collate xs = Map.toList (Map.fromListWith (++) [ (k,[v]) | (k,v) <- xs ])

generateFile :: Show a => String -> a -> IO ()
generateFile tag a = do
  let fp :: FilePath = "gen/" ++ tag ++ ".out"
  putStrLn $ "Writing file: " <> fp
  writeFile fp (show a)
