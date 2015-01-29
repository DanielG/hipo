{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP, FlexibleContexts, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables #-}
module HostListParser where

import Data.Char (isUpper)
import Data.Default
import Data.List
import Data.List.Split (splitOn)
import Data.IP
import Data.Functor
import Data.Maybe

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.Trans
import Control.Applicative hiding (many, (<|>), optional)

import Debug.Trace

import Text.Parsec hiding (label, newline, space, State)
import Text.Parsec.Pos

import HostListTokenizer

import Types

twace :: Show a => [Char] -> a -> a
twace l e = trace (concat ["DEBUG: ", l, " ", show e]) e


data State = State {
      sDomain :: [String],
      sIndent :: Int
    } deriving (Show)

instance Default State where
    def = State def def

--type Parser a = Parsec [TokenPos] () a
type Parser = ParsecT [TokenPos] Int (Writer [String])

parse :: String -> String -> Either ParseError HostNet
parse file i = do
  ts <- tokenize file i
  let (a, w) = runWriter (runParserT hostListParser def file ts)
  --trace (unlines w) a
  a

rootNet :: [HostNet] -> HostNet
rootNet = Net (initialPos "<internal>") (Just ":root:") ("0.0.0.0",0) []

extract :: SourcePos -> [TokenPos] -> [TokenPos]
extract ePos input =
    takeWhile np input
    where np tokp = pos tokp /= ePos

wrap :: Show s => String -> s -> Parser a -> Parser a
wrap name args a = do
  level <- getState

  input <- getInput
--  let p = pos . head $ (input ++ [last input])
  lift $ tell [replicate level ' ' ++ "enter " ++ name ++ " " ++ show args]
  modifyState (+2)

 --Stream s m t => ParsecT s u m a -> u -> SourceName -> s -> m (Either ParseError a)
  -- s <- getParserState
  -- let (pr,w) = runWriter $ runParserT (do setPosition pos; r <- a; i <- getInput; return (r,i)) s "" i
  -- r <- case pr of
  --   Left err -> fail $ show err
  --   Right (r,i) -> do setInput i
  --                     lift $ tell w
  --                     return r
  r <- a
  modifyState (subtract 2)
  lift $ tell [replicate level ' ' ++ "exit " ++ name ++ " " ++ show args]

--  lift $ tell ["parsed:\n" ++ concatMap ((++"\n") . show) (extract ePos i)]

  return r

hostListParser :: Parser HostNet
hostListParser = wrap "hostListParser" () $ do
  optional $ try preamble
  void $ many newline
  rootNet <$> manySameIndent 0 decl <* eof


manySameIndent :: Int -> (Int -> Parser a) -> Parser [a]
manySameIndent i p = wrap "manySameIndent" (i) $ many (try $ indentEq i >> p i)

indentEq :: Int -> Parser ()
indentEq i = wrap "indentEq" (i) $ do
               i' <- indent
               if (i == i')
                then return ()
                else fail $ msg i'
    where msg i' = "Indent should be equal to " ++ show i
                ++ " but is " ++ show i' ++ " really."

preamble :: Parser [TokenPos]
preamble = wrap "preamble" () $ anyToken `manyTill` (preambleEnd >> newline)

decl :: Int -> Parser HostNet
decl i = wrap "decl" (i) $
         (try (host i) <|> try (net i) <|> fail "no declaration found")

host :: Int -> Parser HostNet
host i = wrap "host" (i) $ do
  pos <- getPosition
  ip <- validIp
  ds <- domains1
  m <- (optionMaybe validMac)
  newline
  rs <- concat <$> many record

  optional $ lookAhead $ checkHostIndent i

  return $ Host pos ip ds m rs


checkHostIndent :: Int -> Parser ()
checkHostIndent i = wrap "checkHostIndent" (i) $ do
  i' <- indent
  if i' > i
    then fail "Hosts can't have subnets"
    else return ()

net :: Int -> Parser HostNet
net i = wrap "net" (i) $ do
  pos <- getPosition
  l  <- optionMaybe $ try label
  ip <- validIpNet
  ds <- domains
  newline

  optional $ lookAhead $ checkNetIndent i
  s <- manySameIndent (i+2) decl

  return $ Net pos l ip ds s

checkNetIndent :: Int -> Parser ()
checkNetIndent i = wrap "checkNetIndent" (i) $ do
  i' <- indent
  if (i' - i) > 2
    then fail $ "Line should be indented " ++ show (i+2)
             ++ " but is " ++ show i'
    else return ()

domains1 :: Parser [String]
domains1 = wrap "domains1" () $ validDomain `sepBy1` op ","

domains :: Parser [String]
domains = wrap "domains" () $ validDomain `sepBy` op ","

record :: Parser [Record]
record = wrap "record" () $ do
           optional space
           pos <- getPosition
           os  <- recordOptions
           t   <- field `suchThat` all isUpper
           ds  <- domains
           newline
           return $ map (\d -> Record pos t d os) ds

recordOptions :: Parser [Either Int String]
recordOptions = wrap "recordOptions" () $ many recordOption

recordOption :: Parser (Either Int String)
recordOption = wrap "recordOption" () $
      (Left . read <$> try (field `isValid ` num))
  <|> (Right <$> try (validDomainLabel `suchThat` all (not.isUpper)))
  <|> fail "invalid record option"

suchThat :: MonadPlus m => m a -> (a -> Bool) -> m a; infix 9 `suchThat`
suchThat m p = do x <- m; guard (p x); return x

num :: Tokenizer String
num = many1 digit

label :: Parser String
label = wrap "label" () $ intercalate " " <$> (many1 field) <* op ":"

-- Parsers for verifying the content of a Field to be a certain type

validIp :: Parser IP
validIp          = wrap "validIp" () $
    (field `isValid` ip          <?> "valid IP")
validIpNet :: Parser (IP,Int)
validIpNet       = wrap "validIpNet" () $
    (field `isValid` ipNet       <?> "valid IP/prefix")
validMac :: Parser String
validMac         = wrap "validMac" () $
    (field `isValid` mac         <?> "valid MAC")
validDomain :: Parser String
validDomain      = wrap "validDomain" () $
    (field `isValid` domain      <?> "valid Domain")
validDomainLabel :: Parser String
validDomainLabel = wrap "validDomainLabel" () $
    (field `isValid` domainLabel <?> "valid Domain Label")

isValid :: (Show s,
            Show t,
            Stream s Identity t,
            Default u1)
        => Parser s
        -> ParsecT s u1 Identity b
        -> Parser b
isValid parser verifier = wrap "isValid" () $ do
  f <- parser
  pos <- getPosition
  case runP (verifier <* eof) def "" f of
    Left e -> do
      lift $ tell ["isValid failed in (" ++ (show f) ++ ")"
                  ++ " at " ++ show pos ++ "is(In)Valid " ++ (show e)]
      fail $ "in " ++ (show f) ++ " is(In)Valid " ++ (show e)
    Right r -> return r


ipNet :: Tokenizer (IP,Int)
ipNet = do (addr :: Maybe IP) <- (readMaybe <$> (many1 (digit <|> oneOf ".")))
           (prefix :: Maybe Int) <- (readMaybe <$> (char '/' >> many1 digit))
           case liftA2 (,) addr prefix of
             Just r -> return (r :: (IP,Int))
             Nothing -> fail $ msg addr prefix
    where
      msg a p = "Not an IPv4/prefix (" ++ show a ++ "/" ++ show p ++ ")"


ip :: Tokenizer IP
ip = do addr <- many1 (digit <|> char '.')
        let a :: [(IPv4, String)]
            a = reads addr
        case a of
             ((i,_):_) -> return $ IPv4 i
             [] -> fail ("Not an IPv4 Address (" ++ show addr ++ ")")

prefix :: Parsec String u Int
prefix = char '/' >> (read <$> many1 digit) <?> "prefix"

mac :: Tokenizer String
mac = do ps <- count 2 hexDigit `sepBy1` (char ':')
         (guard $ (length ps) == 6) <?> "valid MAC address"
         return $ intercalate ":" ps

domainLabel :: Tokenizer String
domainLabel = many (alphaNum <|> oneOf "-_")

domain :: Tokenizer String
domain =  intercalate "." <$> domainLabel `sepBy1` char '.'

readMaybe a = fmap fst $ listToMaybe $ reads a

-- Low Level Token Parsers

#define TOK1(cst) \
  (\ t -> case tok t of cst x -> Just x ; _ -> Nothing)
#define TOK0(cst) \
  (\ t -> case tok t of cst   -> Just (); _ -> Nothing)


token_ :: Stream s m TokenPos => (TokenPos -> Maybe a) -> ParsecT s u m a
token_ t = tokenPrim (show.show.tok) nextPos t
    where
      nextPos _ x _  = pos x

op_ :: Parser String
op_ = token_ TOK1(Operator)

op :: String -> Parser ()
op o = do o' <- op_; guard (o == o')

indent :: Parser Int
indent      = token_ TOK1(Indent) <?> "indentation"

field :: Parser String
field       = token_ TOK1(Field)

preambleEnd :: Parser String
preambleEnd = token_ TOK1(PreambleEnd)

space, newline, newline_ :: Parser ()
space       = token_ TOK0(Space)
newline_     = token_ TOK0(Newline)
newline = newline_ <|> eof
