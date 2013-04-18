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

import Types
import HostListTokenizer

data State = State {
      sDomain :: [String],
      sIndent :: Int
    } deriving (Show)

instance Default State where
    def = State def def

type Parser a = Parsec [(Token,SourcePos)] () a

#define TOK(c) token show snd (\(t,_) -> case t of c x -> Just x; _ -> Nothing)
#define TOK_(c) token show snd (\(t,_) -> case t of c -> Just (); _ -> Nothing)

op_     = TOK(Operator)
num     = TOK(Number)
field   = TOK(Field)
indent_ = TOK(Indent)
preambleEnd = TOK(PreambleEnd)
comment = TOK(Comment)

space   = TOK_(Space)
newline = TOK_(Newline)


indent = do i <- (optionMaybe indent_)
            return $ fromMaybe 0 i

op :: String -> Parser ()
op o = do o' <- op_
          guard (o == o')

parser :: Parser ()
parser = do op_; field; num; indent; newline; comment; preambleEnd; return ()

hostListParser :: Parser [HostNet]
hostListParser = do
  p <- preamble
  n <- many (decl 0)
  eof
  return n

preamble = anyToken `manyTill` preambleEnd

decl i = (net i <|> host i)

host :: Int -> Parser HostNet
host i = do
  ip <- validIp
  d  <- validDomain
  m  <- (optionMaybe validMac)
  rs <- many record
  newline
  return $ Host ip d m rs

net i = do
  ip <- validIpNet
  d <- (optionMaybe validDomain)
  newline
  s <- manySameIndent (i+2) decl
  return $ Net ip d s


domains = validDomain `sepBy1` op ","

-- data Record = Record {
--       rType   :: String,
--       rDomain :: [String],
--       rOpts   :: [(String, String)]
--   } deriving (Eq, Show)


record :: Parser Record
record = do newline
            optional space
            liftM3 Record field domains recordOptions


recordOptions :: Parser [(String, String)]
recordOptions =     (return.(,) "num"  <$> field)
                <|> (return.(,) "host" <$> validDomainLabel)
                <|> return []



--liftM3 Net validIpNet  (return [])

-- ^ `manySameIndent p' parses indent before p while the level is equal to `i'
manySameIndent i p = many $ try (indentEq i >> p i)
indentEq i = do i' <- indent; guard (i == i'); return i

-- parser :: Hl ()
-- parser = do
-- --  preamble
--   many1 (net <|> host)

-- net = undefined

-- host = undefined


--ip = do Field i <- field
--        let bleh = read i :: IP

--ipNet = do Field i <- field

--Parsec tok st a -> st -> SourceName -> [tok] -> Either ParseError a
-- Stream s Identity t =>
-- Parsec s u a -> u -> SourceName -> s -> Either ParseError a

isValid :: (Stream s Identity t, Stream s' Identity t', Default u)
        => Parsec s u s' -> Parsec s' u b -> Parsec s u b
isValid p l = try $ do f <- p
                       case runP l def "" f of
                         Left e -> fail (show e)
                         Right r -> return r

validIp = field `isValid` ip
validIpNet = field `isValid` ipNet
validMac = field `isValid` mac
validDomain = field `isValid` domain
validDomainLabel = field `isValid` domainLabel


ipNet :: Parsec String () NetAddr
ipNet = liftM2 NetAddr ip prefix

ip :: Parsec String () IP
ip = do addr <- many1 (digit <|> char '.')
        case reads addr of
          ((i,_):_) -> return i
          [] -> fail "Not an IP"

prefix = char '/' >> num'

mac :: Parsec String () String
mac = do ps <- hexPair `sepBy1` (char ':')
         (guard $ (length ps) == 6) <?> "valid MAC address"
         return $ intercalate ":" ps

hexPair = count 2 hexDigit

num' = read <$> many1 digit


-- The Internet standards (Request for Comments) for protocols mandate that
-- component hostname labels may contain only the ASCII letters 'a' through 'z'
-- (in a case-insensitive manner), the digits '0' through '9', and the hyphen
-- ('-'). The original specification of hostnames in RFC 952, mandated that
-- labels could not start with a digit or with a hyphen, and must not end with a
-- hyphen. However, a subsequent specification (RFC 1123) permitted hostname
-- labels to start with digits. No other symbols, punctuation characters, or
-- white space are permitted.
domainLabel :: Parsec String () String
domainLabel = do a <- letter
                 as <- many (alphaNum <|> char '-')
                 return (a:as)

domain :: Parsec String () String
domain =  intercalate "." <$> domainLabel `sepBy1` char '.'

