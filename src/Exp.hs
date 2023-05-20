
module Exp (nrefsOfAssigns,nrefsOfExp,eNot,eAnd,eOr,eIte,subWire,subNode) where

import Data.Set (Set)
import Assigns (AssignDef(..),WireId(..),NodeId(..),Exp(..))
import qualified Data.Set as Set

nrefsOfAssigns :: [AssignDef] -> Set NodeId
nrefsOfAssigns assigns = do
  let defs = [ n | AssignDef n _ <- assigns ]
  let refs = [ n | AssignDef _ e <- assigns, n <- nrefsOfExp e ]
  Set.fromList (defs ++ refs)

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
      EConst{} -> acc

subWire :: (WireId -> Exp) -> Exp -> Exp
subWire f = trav
  where
    trav = \case
      ENode n -> ENode n
      EWire w -> f w
      ENot x -> eNot (trav x)
      EAnd x y -> EAnd (trav x) (trav y)
      EOr x y -> EOr (trav x) (trav y)
      EXor x y -> EXor (trav x) (trav y)
      EIte x y z -> EIte (trav x) (trav y) (trav z)
      EConst b -> EConst b

subNode :: (NodeId -> Exp) -> Exp -> Exp
subNode f = trav
  where
    trav = \case
      ENode n -> f n
      EWire w -> EWire w
      ENot x -> eNot (trav x)
      EAnd x y -> eAnd (trav x) (trav y)
      EOr x y -> eOr (trav x) (trav y)
      EXor x y -> eXor (trav x) (trav y)
      EIte x y z -> eIte (trav x) (trav y) (trav z)
      EConst b -> EConst b

eNot :: Exp -> Exp
eNot = \case
  ENot e -> e
  EConst b -> EConst (not b)
  e -> ENot e

eAnd :: Exp -> Exp -> Exp
eAnd x y = case (x,y) of
  (EConst True, other) -> other
  (other, EConst True) -> other
  (EConst False, _) -> EConst False
  (_, EConst False) -> EConst False
  _ -> EAnd x y

eOr :: Exp -> Exp -> Exp
eOr x y = case (x,y) of
  (EConst False, other) -> other
  (other, EConst False) -> other
  (EConst True, _) -> EConst True
  (_, EConst True) -> EConst True
  _ -> EOr x y

eXor :: Exp -> Exp -> Exp
eXor x y = case (x,y) of
  (EConst False, other) -> other
  (other, EConst False) -> other
  (EConst True, other) -> eNot other
  (other, EConst True) -> eNot other
  _ -> EXor x y

eIte :: Exp -> Exp -> Exp -> Exp
eIte x y z = case x of
  EConst True -> y
  EConst False -> z
  _ -> EIte x y z
