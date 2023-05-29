
module Compile (main,compile,Prog(..),Comb(..),Func(..),Atom(..),Var) where

import Assigns (NodeId,Exp(..))
import Control.Monad (ap,liftM)
import Data.Map (Map)
import Data.Set (Set)
import EmuState (fixedInputs)
import GetLogic (getLogic,Logic(..),Version(..))
import NodeNames (ofName)
import Text.Printf (printf)
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: Version -> IO () -- dev entry
main version = do
  logic <- getLogic version
  --let _ = simGivenLogic logic
  prog <- compile logic
  print prog

----------------------------------------------------------------------

compile :: Logic -> IO Prog
compile logic = runGen (genFromLogic logic)

genFromLogic :: Logic -> Gen ()
genFromLogic Logic{m} = do

  sequence_ [ GShare (ofName x) (fInput x) | x <- varyingInputs ]
  sequence_ [ GShare (ofName x) (fConst b) | (x,b) <- fixedInputs ]
  sequence_ [ output (ofName x)            | x <- need ]

  where
    varyingInputs = [ "clk0", "res"] ++ ["db"++show i | i <- [0::Int .. 7]]

    need :: [String]
    need = []
      ++ ["sync","rw"]
      ++ ["ab"++show i | i <- [0::Int .. 15]]
      ++ ["p"++show i | i <- [0::Int .. 7]]
      ++ ["ir"++show i | i <- [0::Int .. 7]]
      -- ++ ["db"++show i | i <- [0::Int .. 7]]
      ++ varyingInputs

    output :: NodeId -> Gen ()
    output n = do
      f <- genN n
      GSetOutput n f

    look :: NodeId -> Exp
    look n = maybe err id $ Map.lookup n m
      where err = error (show ("genN",n))

    genN :: NodeId -> Gen Func
    genN n = do
      GLookup n >>= \case
        Just f -> pure f
        Nothing -> do
          GAmDoing n >>= \case
            True -> GUseLastState n
            False -> do
              GMarkAsDoing n $ do
                f <- genE (look n)
                GNeedState n >>= \case
                  False -> pure ()
                  True -> GSetNextState n f
                GShare n f
                pure f

    genE :: Exp -> Gen Func
    genE = \case
      ENode n -> do
        genN n
      EWire{} -> do
        error "genE/wire"
      EOr e1 e2 -> do
        a1 <- genE e1
        a2 <- genE e2
        fOr a1 a2
      EAnd e1 e2 -> do
        a1 <- genE e1
        a2 <- genE e2
        fAnd a1 a2
      EXor e1 e2 -> do
        a1 <- genE e1
        a2 <- genE e2
        fXor a1 a2
      EConst{} -> do
        undefined -- ???
      EIte e1 e2 e3 -> do
        a1 <- genE e1
        a2 <- genE e2
        a3 <- genE e3
        fIte a1 a2 a3
      ENot e -> do
        a <- genE e
        pure (fNot a)


----------------------------------------------------------------------

instance Functor Gen where fmap = liftM
instance Applicative Gen where pure = GRet; (<*>) = ap
instance Monad Gen where (>>=) = GBind

data Gen a where
  GRet :: a -> Gen a
  GBind :: Gen a -> (a -> Gen b) -> Gen b
  GIte :: Func -> Func -> Func -> Gen Func

  GShare :: NodeId -> Func -> Gen ()
  GLookup :: NodeId -> Gen (Maybe Func)

  GMarkAsDoing :: NodeId -> Gen a -> Gen a
  GAmDoing :: NodeId -> Gen Bool

  GNeedState :: NodeId -> Gen Bool
  GUseLastState :: NodeId -> Gen Func
  GSetNextState :: NodeId -> Func -> Gen ()
  GSetOutput :: NodeId -> Func -> Gen ()


data State = State { u :: Int, env :: Map NodeId Func, regs :: Set NodeId }

runGen :: Gen () -> IO Prog
runGen g0 = finalize <$> loop doing0 s0 g0 k0
  where

    k0 :: State -> () -> IO Prog
    k0 _ () = pure PDone

    doing0 :: Set NodeId = Set.empty

    s0 :: State = State { u = 1, env = Map.empty, regs = Set.empty }

    loop :: Set NodeId -> State -> Gen a -> (State -> a -> IO Prog) -> IO Prog
    loop doing s@State{env,regs,u} g k = case g of
      GRet x -> k s x
      GBind g f -> loop doing s g $ \s x -> loop doing s (f x) k
      GIte i t e -> do
        let v = Var u
        PLet v (CombIte i t e) <$> (k s { u = u+1 } (fVar v))

      GShare n f -> do
        --print ("GShare",n)
        k s { env = Map.insert n f env } ()

      GLookup n -> do
        --print ("GLookup",n)
        k s (Map.lookup n env)

      GMarkAsDoing n g -> loop (Set.insert n doing) s g k
      GAmDoing n -> k s (n `Set.member` doing)

      GNeedState n -> k s (n `Set.member` regs)
      GUseLastState n -> k s { regs = Set.insert n regs } (fReg n)

      GSetNextState n f -> do -- can/should we collect at end?
        PSetNext n f <$> k s ()

      GSetOutput n f -> do -- can/should we collect at end?
        PSetOutput n f <$> k s ()

----------------------------------------------------------------------

fAnd,fOr,fXor :: Func -> Func -> Gen Func
fIte :: Func -> Func -> Func -> Gen Func
fConst :: Bool -> Func

fOne,fZero :: Func
fInput :: String -> Func
fVar :: Var -> Func
fReg :: NodeId -> Func
fNot :: Func -> Func

fAnd x y = fIte x y fZero
fOr x y = fIte x fOne y
fXor x y = fIte x (fNot y) y
fConst = \case True -> fOne; False -> fZero

fIte i t e =
  case i of
    Pos AOne -> pure t
    Neg AOne -> pure e
    _ ->
      case (t,e) of
        (Pos AOne, Neg AOne) -> pure i
        (Neg AOne, Pos AOne) -> pure (fNot i)
        (f1,f2) | f1 == f2 -> pure f1
        _ -> GIte i t e


fOne = Pos AOne
fZero = Neg AOne
fInput x = Pos (AInput x)
fReg n = Pos (AReg n)
fVar v = Pos (AVar v)

fNot = \case
  Pos a -> Neg a
  Neg a -> Pos a

----------------------------------------------------------------------

finalize :: Prog -> Prog
finalize prog = PWithState regs prog
  where
    regs = collect [] prog
    collect acc = \case
      PDone -> acc
      PLet _ _ p -> collect acc p
      PSetNext n _ p -> collect (n:acc) p
      PSetOutput _ _ p -> collect acc p
      PWithState{} -> error "finalize"

----------------------------------------------------------------------

data Prog
  = PDone
  | PLet Var Comb Prog
  | PSetNext NodeId Func Prog
  | PSetOutput NodeId Func Prog
  | PWithState [NodeId] Prog -- just one line at top of prog

data Comb = CombIte Func Func Func

data Func = Pos Atom | Neg Atom
  deriving Eq

data Atom = AOne | AVar Var | AInput String | AReg NodeId
  deriving Eq

newtype Var = Var Int
  deriving (Eq,Ord)

----------------------------------------------------------------------

instance Show Prog where show prog = unlines (ppProg prog)

ppProg :: Prog -> [String]
ppProg = \case
  PWithState regs p -> printf "#regs = %d : %s\n" (length regs) "(show regs)" : ppProg p
  PDone -> []
  PLet v c p -> printf "let %s = %s" (show v) (show c) : ppProg p
  PSetNext n a p -> printf "set %s = %s" (show n) (show a) : ppProg p
  PSetOutput n a p -> printf "output %s = %s" (show n) (show a) : ppProg p
  --PSetNext _ _ p -> ppProg p -- TEMP, hide

instance Show Comb where
  show = \case
    CombIte i t e -> printf "%s ? %s : %s" (show i) (show t) (show e)

instance Show Func where
  show = \case
    Pos a -> show a
    Neg AOne -> "0"
    Neg a -> "!"++show a

instance Show Atom where
  show = \case
    AOne -> "1"
    AVar v -> show v
    AReg n -> show n
    AInput s -> s

instance Show Var where
  show (Var u) = printf "u%d" u

