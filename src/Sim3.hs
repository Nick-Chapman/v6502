
module Sim3 ( simGivenLogic ) where  -- compile & sim

import Compile (Prog(..),Comb(..),Func(..),Atom(..),Var,compile)
import Data.Map (Map)
import EmuState (Sim(..),CycleKind(..),State,Inputs,makeState,updateState,applyInputs,lookState,posClk,negClk,resetHI,resetLO,setInputByte,getAB,getRW,getDB)
import GetLogic (Logic(..))
import Text.Printf (printf)
import qualified Data.Map as Map


simGivenLogic :: Logic -> IO Sim
simGivenLogic logic = do
  prog <- compile logic
  generateFile "prog" prog
  pure $ simGivenProg prog


simGivenProg :: Prog -> Sim
simGivenProg prog = do

  let s0 = initState prog
  stabDuringResetPermissive (dbz ++ posClk) s0 $ \s1 -> do
  stabDuringReset (negClk) s1 $ \s2 -> do
  stabDuringReset (dbz ++ posClk) s2 $ \s3 -> do
  stabDuringReset (negClk) s3 $ \s4 -> do
  loop s4

  where

    dbz = [] --setInputByte "db" 0 -- needed on posClk during reset & writeCycle

    loop :: State -> Sim
    loop s0 = do
      -- We collect the r/w line and address bus after a neg clock edge
      let addr = getAB s0
      let kind = if (getRW "sim2/simGivenLogic" s0) then ReadCycle else WriteCycle
      Decide addr kind $ do
      case kind of

        ReadCycle -> do
          -- Present the byte read from memory when clock is High.
          ReadMem addr $ \byte -> do
          let dbi = setInputByte "db" byte
          stab (dbi ++ posClk) s0 $ \s1 -> do
          NewState (applyInputs dbi s1) $ do -- feels a bit hacky
          stab (negClk) s1 $ \s2 -> do
          NewState s2 $ do
          loop s2

        WriteCycle -> do
          -- Collect the byte to be written to memory when clock is High.
          stab (dbz ++ posClk) s0 $ \s1 -> do
          NewState s1 $ do
          let byte = getDB s1
          WriteMem addr byte $ do
          stab (negClk) s1 $ \s2 -> do
          NewState s2 $ do
          loop s2

    stab                      = stabG Strict resetHI
    stabDuringReset           = stabG Strict resetLO
    stabDuringResetPermissive = stabG Permissive resetLO

    stabG :: StabMode -> Inputs -> Inputs -> State -> (State -> Sim) -> Sim
    stabG mode reset i s k = do
      let (iopt,s') = stabilize mode prog (-- fixedInputs ++
                                           reset ++ i) s
      Stabilization iopt $ do
      k s'


data StabMode = Strict | Permissive

stabilize :: StabMode -> Prog -> Inputs -> State -> (Maybe Int,State)
stabilize mode prog inputs s0 = loop 0 s0
  where
    max = 50
    err = error (show ("failed to stabilize in",max))
    loop :: Int -> State -> (Maybe Int, State)
    loop i s1 = do
      let s2 = oneStep prog inputs s1
      if s1 == s2 then (Just i, s1) else
        if i == max then (case mode of Strict -> err; Permissive -> (Nothing,s1)) else
          loop (i+1) s2

oneStep :: Prog -> Inputs -> State -> State
oneStep prog inputs s = runProg prog inputs s


initState :: Prog -> State
initState = \case
  PWithState _regs _ -> makeState [ (n,False) | n <- _regs ]
  _ -> undefined

generateFile :: Show a => String -> a -> IO ()
generateFile tag a = do
  let fp :: FilePath = "gen/" ++ tag ++ ".out"
  --putStrLn $ "Writing file: " <> fp
  writeFile fp (show a)

runProg :: Prog -> Inputs -> State -> State
runProg prog inputs s0 = do
  let b0 :: Binds = Map.empty
  --applyInputs inputs
  (loop b0 s0 prog)
  where

    iMap :: Map String Bool
    iMap = Map.fromList inputs

    loop :: Binds -> State -> Prog -> State
    loop b s = \case
      PWithState _ prog -> loop b s prog
      PDone -> s
      PLet var comb prog ->
        loop (Map.insert var (evalC b s comb) b) s prog
      PSetNext n f prog ->
        loop b (updateState s (n, evalF b s f)) prog

      PSetOutput n f prog ->
        loop b (updateState s (n, evalF b s f)) prog

    evalC :: Binds -> State -> Comb -> Bool
    evalC b s = \case
      CombIte i t e ->
        if (evalF b s i) then (evalF b s t) else (evalF b s e)

    evalF :: Binds -> State -> Func -> Bool
    evalF b s = \case
      Pos a -> evalA b s a
      Neg a -> not (evalA b s a)

    evalA :: Binds -> State -> Atom -> Bool
    evalA b s = \case
      AOne -> True
      AVar v ->
        maybe err id $ Map.lookup v b
        where err = error (printf "evalA/Var: %s" (show v))
      AInput i  ->
        maybe err id $ Map.lookup i iMap
        where err = False --error (printf "evalA/AInput: %s" (show i))
      AReg n ->
        lookState s n


type Binds = Map Var Bool
