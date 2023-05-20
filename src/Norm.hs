
module Norm (normalize) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Exp (eNot,eOr,eAnd,eIte,subWire)
import Assigns (Line(..),AssignDef(..),WireDef(..),MuxDef(..),WireId,Exp(..),Vec(..))
import Misc (the)

----------------------------------------------------------------------
-- normalize

normalize :: [Line] -> [AssignDef]
normalize logic0 = do
  let logic1 = collateLogic logic0
  let logic2 = inlineWires logic1
  let logic3 = (collateLogic . uncollateLogic) (expandMuxes logic2)
  let Logic{assignDefs=as,wireDefs=ws,muxDefs=ms} = logic3
  case (ws,ms) of
    ([],[]) -> as
    _ -> error "normalize"

----------------------------------------------------------------------
-- expand mux defs

expandMuxes :: Logic -> Logic
expandMuxes Logic{assignDefs=as,wireDefs=ws,muxDefs=ms} = do
  let as2 = map expandMuxDef ms
  Logic{assignDefs=as++as2,wireDefs=ws,muxDefs=[]}

expandMuxDef :: MuxDef -> AssignDef
expandMuxDef MuxDef{width=w,name=_,o,i,s=Vec ss,d=Vec ds} =
  if (length ss /= w) then error "vec-s" else
    if (length ds /= w) then error "vec-d" else do
      --assign o = (|s) ? &(d|(~s)) : i;
      let cond = foldl1 eOr ss
      let exp = foldl1 eAnd [ eOr d (eNot s) | (s,d) <- zip ss ds ]
      AssignDef o (eIte cond exp i)

----------------------------------------------------------------------
-- inline wire defs

inlineWires :: Logic -> Logic
inlineWires logic = do
  let ws = wrefsOfLogic logic
  foldl inlineWire logic ws

inlineWire :: Logic -> WireId -> Logic
inlineWire Logic{assignDefs=as,wireDefs=ws,muxDefs=ms} wToBeInlined = do
  let wBody = the "inlineWire" [ e | WireDef w e <- ws, w == wToBeInlined ]

  let f = subWire (\w -> if w == wToBeInlined then wBody else EWire w)
  let fa (AssignDef x e) = AssignDef x (f e)
  let fw (WireDef x e) = WireDef x (f e)
  let fv (Vec xs) = Vec (map f xs)
  let fm m@MuxDef{i,s,d} = m { i = f i, s = fv s, d = fv d }

  let as' = [ fa x | x <- as ]
  let ws' = [ fw x | x@(WireDef w _) <- ws, w /= wToBeInlined ]
  let ms' = [ fm x | x <- ms ]
  Logic{assignDefs=as',wireDefs=ws',muxDefs=ms'}

wrefsOfLogic :: Logic -> [WireId]
wrefsOfLogic logic = [ w | e <- expsOfLogic logic, w <- wrefsOfExp e ]

wrefsOfExp :: Exp -> [WireId]
wrefsOfExp = loop []
  where
    loop acc = \case
      ENode{} -> acc -- AGGHH, was bug! []
      EWire w -> w : acc
      ENot x -> loop acc x
      EAnd x y -> loop (loop acc x) y
      EOr x y -> loop (loop acc x) y
      EXor x y -> loop (loop acc x) y
      EIte x y z -> loop (loop (loop acc x) y) z
      EConst{} -> acc

----------------------------------------------------------------------
-- Logic

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

uncollateLogic :: Logic -> [Line]
uncollateLogic Logic{assignDefs=as,wireDefs=ws,muxDefs=ms} =
  ( []
    ++ map LineA as
    ++ map LineW ws
    ++ map LineM ms
  )

collateLogic :: [Line] -> Logic
collateLogic lines = Logic
  { assignDefs = sortBy (comparing aLeft) [ x | LineA x <- lines ]
  , wireDefs = sortBy (comparing wLeft) [ x | LineW x <- lines ]
  , muxDefs = [ x | LineM x <- lines ]
  }
