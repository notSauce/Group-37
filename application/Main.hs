{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parse
import Insert
import Retrieve
[-- Maybe seperate the crime types into a seperate module or the weather to then later evaluate into the graph or database
import Remove
import System.Environment
import Database.SQLite3
import GHC.Int
import Data.ByteString.Lazy.Char8 as S
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

{- 

Ask for input to then feed it into relevent functions to allow the store of the data/information 
-}

processCia :: Database 
           -> [String] -- ^ list of 'crimeTypesChartData-table' 
           -> IO() 
           
processCia db cias = do
    Prelude.putStrLn $ "What data are you interested in? " ++ show(cias)
   
    cia <- getLine
    Prelude.putStrLn $ "How do you wish to view or if you would like to remove? [Database] [Graph] [Remove]"
    infomation <- getLine
   
   if cia `Prelude.elem` cias && infomation=="Database" then
        do
            [-- I forgot how to print out on a newline :( 
            print $ "Status: \n"  ++ "-> Processing " ++ cia
            
            response <- getCrypto cia
            let cia2 = Prelude.drop 4 $ cia
            let crime = parse cia2 (unpack response)
            batchInsert cia2 crime
            Prelude.putStrLn $ "Database has been created! Please check the current folder."
    
    else if cia `Prelude.elem` cias && infomation=="Remove" then
        do
            Prelude.putStrLn $ "Continue to delete the entire database or a particular entry? [Entry] [Database]"
            response <- getLine
            
            if response == "Database" then 
               do
                 deleteData db (Prelude.drop 4 $ cia)
            
            else if response == "Entry" then 
               do
                  Prelude.putStrLn $ "What date do you want to delete the paticular entry for? [MM]"
                  date <- getLine
                  deleteForDate db date
            
            else
               Prelude.putStrLn $ "Selected: delete data voided"
    else if cia `Prelude.elem` cias && infomation=="Graph" then
        do
            Prelude.putStrLn $ "Select graph to create? [Month] [Total] [Type] [Percentage]"
            infomation <- getLine
            
           [--                        !!!        NOTE:          !!!
           [-- Might have to rethink using two data-tables as there is a overlap so maybe a pointer is 
           [-- needed to continue and succesfully use "Type" & "Percentage" as these are not from the
           [-- table "crimeLevelsChartData-table" but from the table "crimeTypesChartData-table"
           
            if (infomation `Prelude.elem` ["Month", "Total", "Type", "Percentage"]) then do
                let graphTitles = [infomation]
                response <- getCrypto cia
                
                let graphValues = parse_graph cia infomation (unpack response)
                let graphName = infomation
                let filename = cia ++ graphName ++ "_graph.png"
                
                toFile def filename $ do
                    layout_title .= graphName ++ " graph for " ++ cia
                    layout_title_style . font_size .= 10
                    layout_x_axis . laxis_generate .= autoIndexAxis (Prelude.map fst graphValues)
                    plot $ fmap plotBars $ bars graphTitles (addIndexes (Prelude.map snd graphValues))
            
            else
                Prelude.putStrLn "Error! Invalid input! Next time please select the avaliable options."

    else
        Prelude.putStrLn $ "Please pick from the list above!"




{-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !!!!!!!!!!        Needs review for later additions as it needs to be overlapped w weather data            !!!!!!!!!

[-- This is the block responsible for when the input is Database to where it will give the 
[-- option to select a crime to process the result into functions to allow the query data
[-- from the database

query :: Database 
           [-- crimeTypesChartData-table
           -> [String] -- ^ list of 'crimeTypesChartData-table' 
           -> IO()  

query db cias = do
    Prelude.putStrLn $ "Select crime to query " ++ show(cias)
    cia <- getLine
    
    if cia `Prelude.elem` cias then
        do
            let cia2 = Prelude.drop 4 $ cia
            response <- getCrypto cia
            
            let bitcoin = parse cia2 (unpack response)
            batchInsert cia2 bitcoin
            
            [-- Might remove the averageType as it is not needed nor makes sense 
            averageType <- averageType db cia2
            averageTotal <- averageTotal db cia2
            
            Prelude.putStrLn $ "Crime type average for " ++ cia ++ " is " ++ (show averageClose)
            Prelude.putStrLn $ "Crime average count for " ++ cia ++ " is " ++ (show averageOpen)
           
            [-- Might remove the averageType as it is not needed nor makes sense 
            Prelude.putStrLn $ "Select a month to view the type of crime in that paticular time. (Jan (01) to Dec (12) )"
            
            date <- getLine
            crimeInf <- crimeInfo db date
            
            Prelude.putStrLn $ "Crime details: " ++ (show crimeInf)
            
            totalVal <- totalForDate db date
            
            [--                             ***           Note:             ***
            [--Again this will not be needed as it doesnt seem any need plus this is just for testing
            [-- was attempting to link the month w the type of crime but it should be left seperated
            [-- or not included at all
            typeVal <- typeForDate db date
            
            Prelude.putStrLn $ "On " ++ date ++ " " ++ cia ++ " total crime count at " ++ (show totalVal) ++ ", has the type of " ++ (show typeVal)
            
            let monthCheck = read (Prelude.take 2 (Prelude.drop 5 date))
            
            [-- Obvs when the weather is mereged the diff will be checking for relation of somesort
            let diff = 1 [-- A more relevent equation/formular will replace this 
            [-- Posibble form could check if there is any relation to if there was a simularity 
            [-- Between the coldness/hotness and the crime count or paticular type of crime
            
            
            [-- Need to remove the 'if diff > 0.0' if no relevent form is found
            if  (monthCheck `Prelude.elem` [1..12]) then if diff > 0.0 then 
                 Prelude.putStrLn $ cia ++ " increased by a total of " ++ (show diff) ++ " on " ++ date
                
                else if diff < 0.0 then
                 Prelude.putStrLn $ cia ++ " decreased by a total of "  ++ (show diff) ++ " on " ++ date
                
                else
                 Prelude.putStrLn $ "Error! No avalible data for this date."
            else 
              Prelude.putStrLn $ "Date entered was invalid! (MM must be between 1-12)"
    else 
        Prelude.putStrLn $ " Invalid cime type entered."
    {-Prelude.putStrLn $ "Do you want to run another query?"
    response <- getLine
    if response == "Y" || response == "Yes" || response == "yes" then 
       query db cias
    else
       Prelude.putStrLn $ "You chose to stop querying. Quitting application..."-}

-- | main function for application which performs IO


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ --}



main :: IO ()
main = do
    db <- database
    initialiseDB db
    let cias = ["Crime1", "Crime2", "Crime3"] [-- Currently holding string crime[] as a holder but it would be good for a weather related stat or type
    processCia db cias
    query db cias
            
            
            
