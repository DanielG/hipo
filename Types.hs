module Types where

import Data.Default
import Data.IP

data Record = Record {
      rType    :: String,
      rDomains :: [String],
      rOpts    :: [(String, String)]
  } deriving (Eq, Show)

instance Default Record where
    def = Record def def def

data NetAddr = NetAddr { nIp :: IP, nPrefix :: Int } deriving (Eq,Show)

data HostNet = Net { nAddr    :: NetAddr,
                     nDomain  :: Maybe String,
                     nSubNets :: [HostNet]
                   }
             | Host { hAddr    :: IP,
                      hDomain  :: String,
                      hMAC     :: Maybe String,
                      hRecords :: [Record]
                    }
             deriving (Eq, Show)

