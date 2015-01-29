import Data.List

import System.Exit
import System.Environment
import System.Console.GetOpt

import Text.Groom

import Types
import HostListParser
import HostListTokenizer

printer :: (Show a) => a -> IO ()
printer = putStrLn . groom

main :: IO ()
main = do
  f <- getContents

  print "tracing"
  let Right t = tokenize ("") f
  mapM_ (\e -> putStrLn $ show e ++ ","  ) t
