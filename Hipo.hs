import HostListTokenizer
import HostListParser

import Text.Parsec
import Text.Groom
import Data.Default

main :: IO ()
main = do
  f <- getContents
  let res = runParser tokenizer def "stdin" f

  putStrLn $ groom $ res

  case res of
    Left e -> putStrLn $ show e
    Right ts -> putStrLn $ groom $ runParser (hostListParser) def "stdin'" ts



--runParser :: GenParser tok st a -> st -> SourceName -> [tok] -> Either ParseError a
--main = do
--  f <- getContents
--  f <- readFile "/home/dxld/srv/IP-Allocation-Plan.txt"
--  print $ runParser ipList def "stdin" f
--  putStrLn $ groom $ runParser ipList def "stdin" f


