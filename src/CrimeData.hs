[-- 
 
 
 Hey Regent can you just fill in these blanks for me :)   
 
 
 
p.s you can remove the brackets after you fill them 
 
 --]

{-!!!!!!!!!!!!! Relevent files need to import this paticular module !!!!!!!!!!-}


{-# LANGUAGE OverloadedStrings #-}
module CrimeData
    ( getCrimeData
    ) where

import Insert
import Network.HTTP.Conduit
import Data.Time.Clock
import Data.Time.Calendar
import qualified Control.Exception as E
import qualified Data.ByteString.Lazy.Internal as L
import Network.HTTP.Types.Status (statusCode)

type URL = String


past :: UTCTime
past = UTCTime (ModifiedJulianDay 56200) (secondsToDiffTime 0)

future :: UTCTime
future = UTCTime (ModifiedJulianDay 562000) (secondsToDiffTime 0)

[-- Downloaded via the cookie for the web link 
cookie :: Cookie
cookie = Cookie { cookie_name = "Fill Insert Cookie Name"
                , cookie_value = "Fill Insert Cookie value"
                , cookie_expiry_time = future
                , cookie_domain = "Fill Insert Domain"
                , cookie_path = "/"
                , cookie_creation_time = past
                , cookie_last_access_time = past
                , cookie_persistent = False
                , cookie_host_only = False
                , cookie_secure_only = False
                , cookie_http_only = False
                }


[-- HTTP request to download dataset

getCrimeData :: URL -> IO L.ByteString
getCrimeData cia = do
    
    request' <- parseRequest $ "https://(Fill links)" ++ cia ++ "?(Fill end part)"
    manager <- newManager tlsManagerSettings
    
    let request = request' { cookieJar = Just $ createCookieJar [cookie] }
    response <- httpLbs request manager
    return $ responseBody response