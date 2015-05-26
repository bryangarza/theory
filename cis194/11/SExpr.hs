{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore = many

oneOrMore :: Parser a -> Parser [a]
oneOrMore = some

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = many (satisfy isSpace)

ident :: Parser String
ident = liftA2 (:) (satisfy isAlpha) (many $ satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

rmSpaces :: Parser a -> Parser a
rmSpaces p = spaces *> p <* spaces

atomInt :: Parser Integer
atomInt = read <$> (some $ satisfy isNumber)

atomEither :: Parser Atom
atomEither = rmSpaces $ I <$> ident <|> N <$> atomInt

leftParen :: Parser Char
leftParen = char '('

rightParen :: Parser Char
rightParen = char ')'

listSExpr :: Parser [SExpr]
listSExpr = rmSpaces $ leftParen *> (many parseSExpr) <* rightParen

parseSExpr :: Parser SExpr
parseSExpr = Comb <$> listSExpr <|> A <$> atomEither
