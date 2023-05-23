
module Top (main) where

import GetLogic (Version(Raw,Simp,Minimal))
import System.Environment (getArgs)
import qualified CBM (main)
import qualified Compile (main)

main :: IO ()
main = do
  config <- parse <$> getArgs
  run config

parse :: [String] -> Config
parse = loop config0
  where
    loop acc = \case
      [] ->acc
      "dev":xs -> loop acc { mode = Dev } xs
      "raw":xs -> loop acc { version = Raw } xs
      "simp":xs -> loop acc { version = Simp } xs
      "min":xs -> loop acc { version = Minimal } xs
      "-max":max:xs -> loop acc { max = read max } xs
      "-trace":xs -> loop acc { trace = True } xs
      args ->
        error (show ("parse",args))

    config0 = Config { mode = CBM, version = Minimal, max = 100, trace = False }


data Config = Config { mode :: Mode, version :: Version, max :: Int, trace :: Bool }

data Mode = CBM | Dev

run :: Config -> IO ()
run Config{mode,version,max,trace} = do
  case mode of
    CBM -> CBM.main version max trace
    Dev -> Compile.main version
