module Types where

import Data.IP
import Data.List
import Data.Maybe
import Control.Applicative

import Text.Parsec.Pos

data Record = Record {
      rPos     :: SourcePos,
      rType    :: String,
      rDomain  :: String,
      rOpts    :: [Either Int String]
  } deriving (Eq, Show)

data HostNet = Net { hnPos      :: SourcePos,
                     hnLabel    :: Maybe String,
                     hnAddr     :: (IP,Int),
                     hnDomain   :: [String],
                     hnSubNets  :: [HostNet]
                   }
             | Host { hPos      :: SourcePos,
                      hAddr     :: IP,
                      hDomain   :: [String],
                      hMAC      :: Maybe String,
                      hRecords  :: [Record]
                    }
             deriving (Eq,Show)

-- Show instances

-- instance Show Record where
--     show (Record _ ty doms opts ) =
--         " " ++ intercalate " " (map snd opts) ++ " "
--             ++ ty ++ " "
--             ++ intercalate ", " doms
--             ++ "\n"

-- instance Show HostNet where
--     show = show' 1

-- ind  :: Int -> String
-- ind i = replicate (i*2) ' '

-- -- Hide the root node otherwise the show output wouldn't parse
-- show' :: Int -> HostNet -> String
-- show' i(Net _ (Just ":root:") _ _ subnets) =
--     (concatMap ((ind i++).show' (i+1)) subnets)

-- show' i (Net _ label ip doms subnets) =
--     "- " ++ fromMaybe "" ((++) <$> label <*> pure ": ")
--          ++ show ip ++ " "
--          ++ intercalate ", " doms ++ "\n"
--          ++ (concatMap ((ind i++).show' (i+1)) subnets)

-- show' i (Host _ ip doms mac records) =
--     "- " ++ show ip ++ " "
--           ++ intercalate ", " doms ++ " "
--           ++ fromMaybe "" mac ++ "\n"
--           ++ (concatMap ((ind i++).show) records)
