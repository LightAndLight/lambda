{-# language OverloadedLists #-}
module Parser where

import Control.Applicative
import Data.Functor
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Token.Highlight

import Expr

idStyle :: CharParsing m => IdentifierStyle m
idStyle =
  IdentifierStyle
  { _styleName = "ident"
  , _styleStart = lower
  , _styleLetter = letter <|> digit
  , _styleReserved = []
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }

identifier :: (Monad m, TokenParsing m) => Unspaced m String
identifier = ident idStyle

constructor :: (Monad m, TokenParsing m) => Unspaced m String
constructor = ident (idStyle { _styleStart = upper })

expr :: (Monad m, TokenParsing m) => m Expr
expr =
  (symbol "\\" $> lam <*> token (runUnspaced identifier) <* symbol "->" <*> expr) <|>
  chainl1 atom (some space $> App)
  where
    atom =
      (Var <$> runUnspaced identifier) <|>
      (Int . read <$> some digit) <|>
      (Ctor <$> runUnspaced constructor)
