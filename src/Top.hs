
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
      "cbm":n:xs -> loop acc { mode = CBM (read n) } xs
      "raw":xs -> loop acc { version = Raw } xs
      "simp":xs -> loop acc { version = Simp } xs
      "min":xs -> loop acc { version = Minimal } xs
      args ->
        error (show ("parse",args))

    config0 = Config { mode = CBM 100, version = Raw }


data Config = Config { mode :: Mode, version :: Version }
data Mode = CBM Int

run :: Config -> IO ()
run Config{mode,version} = case mode of
  CBM n ->  do
    CBM.main version n
    pure ()
