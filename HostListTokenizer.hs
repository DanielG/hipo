module HostListTokenizer(
  Token(..), Tokenizer, tokenizer
  ) where

import Data.List
import Data.IP
import Data.Maybe
import Data.Functor

import Control.Monad
import Control.Applicative hiding (many, (<|>), optional)

import Text.Parsec hiding (label, space, State)
import Text.Parsec.String

import Types

data Token = Operator String
           | Field String
           | Number Int
           | Indent Int
           | Space
           | Newline
           | PreambleEnd String
           | Comment String
           deriving (Eq, Show)

type Tokenizer a = Parsec String () a


injPos :: Tokenizer a -> Tokenizer (a,SourcePos)
injPos e = do p <- getPosition
              a <- e
              return $ (a,p)

tokenizer :: Tokenizer [(Token, SourcePos)]
tokenizer = do
  (choice (map (try.injPos) [
            Indent      <$> indent,
            Space       <$  space,
            Newline     <$  eol,
            PreambleEnd <$>  preambleEnd,
            Comment     <$> comment,
            Operator    <$> operator <* skipMany sp,
            Field       <$> field    <* skipMany sp
--            Number      <$> number   <* skipMany sp
           ])) `manyTill` eof


column = sourceColumn <$> getPosition

column1 = do c <- column
             guard (c == 1)


indent = do length <$> space <* string "- "

space = column1 >> many1 sp

sp = void $ char ' '
eol = void $ many1 newline

preambleEnd = string "---" <* many (char '-')

operator = return <$> oneOf ":,"

field = many1 (alphaNum <|> oneOf ".-/" <|> try (char ':' <* notFollowedBy (char ' ')))

-- number = read <$> many1 digit

comment :: Tokenizer String
comment = char '#' >> anyChar `manyTill` (try newline)


-- parseRecord = RecordType <$> choice (map string ["A","MX","SRV","NS"])
