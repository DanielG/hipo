{-# LANGUAGE RecordWildCards #-}
module Main where --TinyDNSGenerator

import Numeric
import Data.IP
import Data.List
import Data.Word
import Data.Char
import Data.Default
import Data.Serialize as S
import Data.ByteString.Char8 (pack, unpack)
import Data.List.Split
import Control.Monad
import Control.Monad.RWS
import Control.Applicative

import Debug.Trace

import Types
import HostListParser

twace :: (Show s) => s -> s
twace a = twacem "" a

twacem :: (Show s) => String -> s -> s
twacem msg a = trace (msg ++ " " ++ show a) a

type DomainComponents = [String] -- ["at", "dxld", "clients", "eli"]
dc :: DomainComponents -> String
dc = intercalate "." . reverse

splitDomain "" = []
splitDomain d = reverse $ splitOn "." d

toRec (c,i) = c ++ intercalate ":" i ++ "\n"

type TinyGen = RWS DomainComponents String ()

tinyDNSGenerator :: [HostNet] -> String
tinyDNSGenerator hn =
  snd $ evalRWS (genHN hn) def ()

genHN :: [HostNet] -> TinyGen ()
genHN = mapM_ genDecl

genDecl (Net p l a ds sn) = do
  case ds of
    [] -> mapM_ genDecl sn
    _ -> flip mapM_ ds $ \d -> do
             let d' = splitDomain d
             case head d' of
               [] -> local (const $ tail d') $ f d
               _  -> local (++d') $ f d
 where f d = do
         genNet l a d
         mapM_ genDecl sn

genDecl (Host p a ds m r) = do
  flip mapM_ ds $ \d -> do
    let d' = splitDomain d
    case head d' of
      [] -> local (const $ tail d') $ genHost a r
      _  -> local (++d') $ genHost a r

genNet :: Maybe String -> (IP,Int) -> String -> TinyGen ()
genNet label net host = do
  domain <- dc <$> ask
  tell $ toRec ("+",[domain, show $ fst net])

genHost :: IP -> [Record] -> TinyGen ()
genHost ip recs = do
  domain <- dc <$> ask
  tell $ toRec ("=",[domain, show ip])
  mapM_ (genRecord ip) recs

recType :: String -> Either Int String
recType "NS" = Right "."
recType "MX" = Right "@"
recType "SRV" = Left 33

genRecord :: IP -> Record -> TinyGen ()
genRecord ip Record {..} = do
  labels <- ask
  let fqdn = dc labels
  r <- case (rType, rOpts) of
         ("A", []) ->
             return ("=", [fqdn, show ip])
         ("NS",Right ns:[]) ->
             let fqdn = dc $ labels ++ splitDomain rDomain in
             return (".", [fqdn, show ip, ns])
         ("MX",Left distance:[]) ->
             return ("@", [fqdn, show ip, rDomain, show distance])
         ("SRV",Left port:[]) ->
             let fqdn' = dc $ labels ++ (splitDomain rDomain) in
             return (":", [fqdn', "33", encodeRDATA $ encodeSRV 0 0 port fqdn])
         _ -> fail $ "wrong record format at " ++ show rPos
  tell $ toRec r

encodeSRV prio weight port dom =
    unpack $ runPut $ do
      mapM_ (putWord16be . fromIntegral) [prio, weight, port]
      putDomain dom

putDomain dom | (not $ all isAscii dom) = fail "ONLY ASCII PLS!"
putDomain dom = encodeComponent `mapM_` (splitOn "." dom) >> putWord8 0
    where encodeComponent :: String -> Put
          encodeComponent d = do
            putWord8 $ fromIntegral (length d)
            putByteString (pack d)

encodeRDATA (s:sx) | isAlphaNum s = [s] ++ encodeRDATA sx
                   | ord s < 2^8 = encodeChar s ++ encodeRDATA sx
                   | otherwise =
                      error "Invalid character for hostname: '"++show s++"'"
encodeRDATA [] = []

encodeChar c = do
  "\\" ++ replicate (3 - length s) '0' ++ s
 where s = showOct (ord c) ""


--  tell "'" ++ a ++ ":" ++ d ++ ":" ++ show a ++ ":"

main = do
  res <- parse "<stdin>" <$> getContents
  case res of
    Left err -> fail $ show err
    Right (Net _ _ _ _ hn) -> putStrLn $ unlines $ nub $ lines $ tinyDNSGenerator hn
