{-# LANGUAGE OverloadedStrings #-}
module Insert
    ( database
    , initialiseDB
    , batchInsert
    ) where

import Database.SQLite3
import DataTypes
import Data.Text (pack)
import Data.Conduit.Binary (sinkFile) -- Exported from the package conduit-extra
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString as B
import Network.HTTP.Conduit
import Network.HTTP.Types.Status (statusCode)
import Conduit (runConduit, (.|))
import Control.Monad.Trans.Resource (runResourceT)
import qualified Control.Exception as E
import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.ByteString as D

[-- connects to the db
database :: IO Database
database = open $ pack "crime.db"

-- | creates a table for crime
initialiseDB :: Database -> IO ()
initialiseDB db = exec db $ pack "CREATE TABLE IF NOT EXISTS crimeLevelsChartData-table (\
            \currency VARCHAR(40) NOT NULL, \
            \state VARCHAR(50) NOT NULL, \
            \fractional_unit VARCHAR(40) NOT NULL, UNIQUE (iso_code));\
            \CREATE TABLE IF NOT EXISTS bitcoin (\
            \currency VARCHAR(40) NOT NULL, \
            \date VARCHAR(40) NOT NULL, \
            \total FLOAT DEFAULT NULL, \
            \percentage FLOAT DEFAULT NULL, \
            \types VARCHAR(40) NULL, \
			\month_close VARCHAR(40) DEFAULT NULL,\


{--
[-- Doesnt work as it hasnt been tested against anything as the weather stuff is needed 

batchInsert :: String 
            -> [CrimetoCompare] -- ^ batchInsert inserts list of [] with data defined type CrimetoCompare 
            -> IO ()

batchInsert crime = do
   db <- database
   initialiseDB db
   
   
 --}
