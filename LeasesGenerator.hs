{-# LANGUAGE RecordWildCards #-}
module Main where

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
import Control.Monad.Reader
import Control.Applicative
import System.Environment

import Debug.Trace

import Types
import HostListParser


type MasqGen = Reader String
type MAC = String
type Host = String

p (i,h,m) = intercalate " " $ [m, h, show i]

lesesGenerator :: (IP,Int) -> [HostNet] -> [String]
lesesGenerator net ns = map p $ concatMap (genNets net) ns

genNets :: (IP,Int) -> HostNet -> [(IP,Host,MAC)]
genNets net (Net _ _ a d sn) =
    if net == a
      then concatMap genNet sn
      else concatMap (genNets net) sn
genNets net _ = []

genNet :: HostNet -> [(IP,Host,MAC)]
genNet (Host _ a d (Just m) _) =
    [(a, "", m)] --head (splitOn "." d) :: String
genNet (Net _ _ _ _ sn) = concatMap genNet sn
genNet _ = []


main = do
  res <- parse "<stdin>" <$> getContents
  [snet,prefix] <- getArgs
  let net = (read snet :: IP, read prefix :: Int)
  case res of
    Left err ->
        fail $ show err
    Right (Net _ _ _ _ hn) ->
        putStrLn $ unlines $ lesesGenerator net hn

-- usage = "hipo-dnsmasq NET < IpAddrList"
