
module Top (main) where

import GetLogic (Version(Raw,Simp,Minimal))
import System.Environment (getArgs)
import qualified CBM (main)

main :: IO ()
main = do
  config <- parse <$> getArgs
  run config

parse :: [String] -> Config
parse = loop config0
  where
    loop acc = \case
      [] ->acc
      "raw":xs -> loop acc { version = Raw } xs -- even slower!
      "simp":xs -> loop acc { version = Simp } xs
      "min":xs -> loop acc { version = Minimal } xs
      "-max":max:xs -> loop acc { max = read max } xs
      "-trace":xs -> loop acc { trace = True } xs
      args ->
        error (show ("parse",args))

    config0 = Config { version = Minimal, max = 100, trace = False }


data Config = Config { version :: Version, max :: Int, trace :: Bool }

run :: Config -> IO ()
run Config{version,max,trace} = do
  CBM.main version max trace
