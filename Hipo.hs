--import IpList

import HostListTokenizer
import HostListParser

import Text.Parsec
import Text.Groom
import Data.Default
import System.Environment

main = do
  f <- getContents
  putStrLn $ groom $ runParser tokenizer def "tok" f
  case runParser tokenizer def "pars" f of
    Left e -> putStrLn $ show e
    Right ts -> putStrLn $ groom $ runParser (indent >> net 0) def "stdin" ts



--runParser :: GenParser tok st a -> st -> SourceName -> [tok] -> Either ParseError a
--main = do
--  f <- getContents
--  f <- readFile "/home/dxld/srv/IP-Allocation-Plan.txt"
--  print $ runParser ipList def "stdin" f
--  putStrLn $ groom $ runParser ipList def "stdin" f


