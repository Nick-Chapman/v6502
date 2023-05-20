
module ParseLogic (parseLogicLines) where

import Assigns (Line(..),MuxDef(..),AssignDef(..),WireDef(..),Vec(..),NodeId(..),Exp(..),WireId(..))
import Par4 (Par,parse,terminated,separated,nl,alts,key,lit,ws0,int,some,sat,many)
import qualified Data.Char as Char

parseLogicLines :: String -> [Line]
parseLogicLines = parse logicGram

logicGram :: Par [Line]
logicGram = terminated nl line
  where
    line = alts [assign,wire,muxInstance]

    muxInstance = do
      key "MUX #("; w <- int; key ") "; x <- identifier
      key " (."; o <- namedArg 'o' oref
      key ", ."; i <- namedArg 'i' exp5
      key ", ."; s <- namedArg 's' vec
      key ", ."; d <- namedArg 'd' vec
      key ");"
      pure (LineM (MuxDef { width = w, name = x, o, i, s, d }))

    namedArg :: Char -> Par a -> Par a
    namedArg c par = do
      lit c
      lit '('
      x <- par
      lit ')'
      pure x

    vec :: Par Vec
    vec = do
      lit '{'
      nibble
      xs <- separated (do lit ','; nibble) exp5
      lit '}'
      pure (Vec xs)

    assign = do
      key "assign"
      nibble
      n <- oref
      nibble
      lit '='
      nibble
      e <- exp5
      lit ';'
      nibble
      eolComment
      pure (LineA (AssignDef n e))

    eolComment = alts [pure (), do
      key "//"
      _ <- many (sat (\c -> c /= '\n'))
      pure ()]

    oref :: Par NodeId
    oref = do lit 'o' ; index

    wire = do
      key "wire "
      w <- WireId <$> identifier
      key " = "
      e <- exp5
      lit ';'
      pure (LineW (WireDef w e))

    exp5,exp4,exp3,exp2,exp1 :: Par Exp

    exp5 = do
      x <- exp4
      alts [pure x
           , do
               lit '?'
               nibble
               y <- exp4
               lit ':'
               nibble
               z <- exp5
               pure (EIte x y z)
           ]

    exp4 = do
      x <- exp3
      alts [pure x
           , do
               lit '^'
               y <- exp4
               pure (EXor x y)
           ]

    exp3 = do
      x <- exp2
      alts [pure x
           , do
               lit '|'
               y <- exp3
               pure (EOr x y)
           ]
    exp2 = do
      x <- exp1
      alts [pure x
           , do
               lit '&'
               nibble
               y <- exp2
               pure (EAnd x y)
           ]

    exp1 = atom

    atom :: Par Exp
    atom = alts [name,invert,bracket]

    invert :: Par Exp
    invert = do
      lit '~'
      x <- atom
      pure (ENot x)

    bracket :: Par Exp
    bracket = do
      lit '('
      e <- exp5
      lit ')'
      nibble
      pure e

    name :: Par Exp
    name = do
      identifier >>= \case
        "i" -> do
          n <- index
          pure (ENode n)
        x -> do
          nibble
          pure (EWire (WireId x))

    nibble :: Par ()
    nibble = ws0

    index :: Par NodeId
    index = do
      lit '['
      i <- int
      lit ']'
      nibble
      pure (NodeId i)

    identifier :: Par String
    identifier = some $ sat idChar

    idChar :: Char -> Bool
    idChar c = Char.isAlpha c || Char.isDigit c || c == '_'
