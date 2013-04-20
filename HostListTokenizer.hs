{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module HostListTokenizer(
  Token(..), TokenPos(..), Tokenizer, tokenizer
  ) where

import Data.Functor

import Control.Monad
import Control.Applicative hiding (many, (<|>), optional)

import Text.Parsec hiding (label, space, State)

data Token = Operator String
           | Field String
           | Indent Int
           | Space
           | Newline
           | PreambleEnd String
           deriving (Eq, Show)

data TokenPos = TokenPos { pos :: SourcePos, tok :: Token } deriving (Eq)

instance Show TokenPos where
    show x = show $ tok x

type Tokenizer a = Parsec String () a

injPos :: Tokenizer Token -> Tokenizer TokenPos
injPos e = do liftM2 TokenPos getPosition e

tokenizer :: Tokenizer [TokenPos]
tokenizer = do
  (choice (map (try.injPos) [
            Indent      <$> indent,
            Space       <$  space,
            Newline     <$  eol,
            PreambleEnd <$> preambleEnd,
            Operator    <$> operator <* skipMany sp,
            Field       <$> field    <* skipMany sp,
            fail "unrecognized token"
    ])) `manyTill` eof

column = sourceColumn <$> getPosition

column1 = do c <- column
             guard (c == 1)

indent = column1 >> length <$> many sp <* string "- "

space = column1 >> many1 sp >> notFollowedBy comment

sp = void $ char ' '
eol = void $ many1 (void newline <|> void (try comment))
comment = skipMany sp >> char '#' >> anyChar `manyTill` newline

preambleEnd = string "---" <* many (char '-')

operator = return <$> oneOf ":,"

field = many1 $ choice [ alphaNum,
                         oneOf ".-/",
                         try (char ':' <* notFollowedBy (char ' ')) ]



