
module Sim2(main) where

import Logic (Logic(..))

main :: Logic -> IO ()
main Logic{name} = do
  print ("sim2",name)
