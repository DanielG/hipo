module Types where

import Data.IP

import Text.Parsec.Pos

data Record = Record {
      rPos     :: SourcePos,
      rType    :: String,
      rDomains :: [String],
      rOpts    :: [(String, String)]
  } deriving (Eq, Show)

--instance Default Record where
--    def = Record (initialPos "") def def def

data NetAddr = NetAddr { nIp :: IP, nPrefix :: Int } deriving (Eq,Show)

data HostNet = Net { nPos     :: SourcePos,
                     nLabel   :: Maybe String,
                     nAddr    :: NetAddr,
                     nDomain  :: Maybe [String],
                     nSubNets :: [HostNet]
                   }
             | Host { hPos     :: SourcePos,
                      hAddr    :: IP,
                      hDomain  :: [String],
                      hMAC     :: Maybe String,
                      hRecords :: [Record]
                    }
             deriving (Eq, Show)

