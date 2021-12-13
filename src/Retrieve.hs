module Retrieve 
    ( totalForDate
    , typeForDate
    , crimeInfo
    ) where

import Database.SQLite3
import DataTypes
import Data.Text (pack, unpack)
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

[-- Query to retrieve open value for an inputted date

openForDate :: Database 
            -> String -- ^ reads in the date(MM) which has type String 
            -> IO Double -- ^ outputs open value which has type IO Double

openForDate db date = do
   stmt <- prepare db (pack $ "SELECT (open) FROM crime WHERE date=:date")
   bindNamed stmt [ (pack ":date", SQLText (pack date)) ]
   result <- step stmt       
   openV <- column stmt 0   
   
   let readOpen (SQLFloat n) = n
       readOpen _ = 0
   let openVal = readOpen openV
   
   return openVal

[-- A query to retrieve close value for an inputted date

closeForDate :: Database 
             -> String -- reads in the date(MM) which has type String
             -> IO Double 

closeForDate db date = do
   
   stmt <- prepare db (pack $ "SELECT (close) FROM bitcoin WHERE date=:date")
   bindNamed stmt [ (pack ":date", SQLText (pack date)) ]
   result <- step stmt       -- one statement step
   closeV <- column stmt 0   -- read how returned
   
   let readClose (SQLFloat n) = n
       readClose _ = 0
   let closeVal = readClose closeV
   return closeVal
   
[-- Retrieves crime details
currencyInfo :: Database 
             -> String
             -> IO [String] -- ^ outputs open value which has type IO String
currencyInfo db date = do
  
  stmt <- prepare db (pack $ "SELECT c.crime FROM crime b JOIN crimeLevelsChartData-table c ON b.crime=c.iso_code WHERE date=:date")
   
   bindNamed stmt [ (pack ":date", SQLText (pack date)) ]
   result <- step stmt       
   
   st <- column stmt 0   
   fr <- column stmt 1   
   cu <- column stmt 2   
   


   let readC (SQLText n) = unpack n
       readC _ = "c.currency"
   let cVal = readC cu
   
   return ["Crime: " ++ cVal]
   
   
   
   
   
