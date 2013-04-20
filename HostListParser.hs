{-# LANGUAGE CPP, FlexibleContexts, MultiParamTypeClasses #-}
module HostListParser where

import Data.Char hiding (Space)
import Data.List
import Data.Word
import Data.IP
import Data.Default
import Data.Maybe
import Data.Functor

import Control.Monad
import Control.Monad.Identity
import Control.Applicative hiding (many, (<|>), optional)

import Debug.Trace

import Text.Parsec hiding (label, newline, space, State)
import Text.Parsec.Pos

import Types
import HostListTokenizer

twace l e = trace (concat ["DEBUG: ", l, " ", show e]) e


data State = State {
      sDomain :: [String],
      sIndent :: Int
    } deriving (Show)

instance Default State where
    def = State def def

type Parser a = Parsec [TokenPos] () a


hostListParser :: Parser [HostNet]
hostListParser = do
  optional $ try preamble
  many newline
  manySameIndent 0 decl <* eof


manySameIndent i p = many $ try (indentEq i >> p i)

indentEq i = do i' <- indent
                if (i == i')
                   then return ()
                   else fail $ "Indent should be equal to " ++ show i
                            ++ " but is " ++ show i' ++ " really."


preamble = anyToken `manyTill` (preambleEnd >> newline)

decl i = (try (host i) <|> try (net i) <|> fail "no declaration found")

host   :: Int -> Parser HostNet
host i = do
  pos <- getPosition
  ip <- validIp
  ds <- domains
  m <- (optionMaybe validMac)
  newline
  rs <- many record

  optional $ lookAhead $ checkHostIndent i

  return $ Host pos ip ds m rs


checkHostIndent i = do
  i' <- indent
  if i' > i
    then fail "Hosts can't have subnets"
    else return ()

net i = do
  pos <- getPosition
  l  <- optionMaybe $ try label
  ip <- validIpNet
  ds <- optionMaybe domains
  newline

  optional $ lookAhead $ checkNetIndent i
  s <- manySameIndent (i+2) decl

  return $ Net pos l ip ds s

checkNetIndent i = do
  i' <- indent
  if (i' - i) > 2
    then fail $ "Line should be indented " ++ show (i+2)
             ++ " but is " ++ show i'
    else return ()


domains = validDomain `sepBy1` op ","

record :: Parser Record
record = do optional space
            pos <- getPosition
            os  <- recordOptions <* notFollowedBy validRecordType
            t   <- field
            ds  <- domains
            return $ Record pos t ds os
            <* newline


recordOptions :: Parser [(String, String)]
recordOptions = many recordOption

recordOption =     ((,) "num"  <$> try (field `isValid` num))
               <|> ((,) "host" <$> validDomainLabel)
               <|> fail "invalid record option"

num :: Tokenizer String
num = many1 digit

label = field <* op ":"

-- Parsers for verifying the content of a Field to be a certain type

validIp          = field `isValid` ip          <?> "valid IP"
validIpNet       = field `isValid` ipNet       <?> "valid IP/prefix"
validMac         = field `isValid` mac         <?> "valid MAC"
validDomain      = field `isValid` domain      <?> "valid Domain"
validDomainLabel = field `isValid` domainLabel <?> "valid Domain Label"

--isValid :: (Stream s Identity t, Stream s' Identity t', Default u)
--        => Parsec s u s' -> Parsec s' u b -> Parsec s u b
isValid parser verifyer = do
  pos <- getPosition
  let name = sourceName pos
      line = sourceLine pos
      col  = sourceColumn pos
      npos = newPos name line col

  f <- parser

  case runP (-- setPosition npos >>
                         verifyer <* eof) def "" f of
    Left e -> fail $ "in " ++ (show f) ++ " is(In)Valid " ++ (show e)
    Right r -> return r

ipNet :: Tokenizer NetAddr
ipNet = liftM2 NetAddr (try ip) prefix

ip :: Tokenizer IP
ip = do addr <- many1 (digit <|> char '.')
        case reads addr of
          ((i,_):_) -> return i
          [] -> fail "Not an IP"

prefix = char '/' >> (read <$> many1 digit) <?> "prefix"

mac :: Tokenizer String
mac = do ps <- count 2 hexDigit `sepBy1` (char ':')
         (guard $ (length ps) == 6) <?> "valid MAC address"
         return $ intercalate ":" ps

domainLabel :: Tokenizer String
domainLabel = do a <- letter
                 as <- many (alphaNum <|> char '-')
                 return (a:as)

domain :: Tokenizer String
domain =  intercalate "." <$> domainLabel `sepBy1` char '.'



-- Low Level Token Parsers

#define TOK1(cst) \
  (\ t -> case tok t of cst x -> Just x ; _ -> Nothing)
#define TOK0(cst) \
  (\ t -> case tok t of cst   -> Just (); _ -> Nothing)

token_ t = token (show.show.tok) pos t

op_ = token_ TOK1(Operator)
op o = do o' <- op_; guard (o == o')

indent      = token_ TOK1(Indent) <?> "indentation"
field       = token_ TOK1(Field)
preambleEnd = token_ TOK1(PreambleEnd)
space       = token_ TOK0(Space)
newline_     = token_ TOK0(Newline)
newline = newline_ <|> eof


