{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Conduit
import Data.String (fromString)
import Data.Aeson
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy.Internal as B

data AllData = AllData { getData :: [SectionData] }
                       deriving (Show)

data SectionData = SectionData { getSection :: Section }
                               deriving (Show)

data Section = Section { getStudents :: [String] }
                       deriving (Show)

-- List of things that are dumb:
--              1. This
instance FromJSON Section where
    parseJSON (Object v) =
        Section <$> v .: "students"
    parseJSON _ = mzero

instance FromJSON AllData where
    parseJSON (Object v) =
        AllData <$> v .: "data"
    parseJSON _ = mzero

instance FromJSON SectionData where
    parseJSON (Object v) =
        SectionData <$> v .: "data"
    parseJSON _ = mzero
-- /dumb list

main = do
    response <- requestSections
    let sections = case parseSections response of
                       (Just x) -> x
                       Nothing -> error "Unable to parse response"
    let studentsInSections = getSectionStudents sections
        numSections        = length studentsInSections
        numStudents        = length $ concat studentsInSections
    print $ (fromIntegral numStudents) / (fromIntegral numSections)

server :: String
server = "https://api.clever.com/v1.1"

sectionsUrl :: String
sectionsUrl = server ++ "/sections"

requestSections :: IO B.ByteString
requestSections = do
    request <- parseUrl sectionsUrl
    let headers  = [(fromString "Authorization", fromString "Bearer DEMO_TOKEN")]
        request' = request { requestHeaders = headers }
    res <- withManager $ httpLbs request'
    return $ responseBody res

parseSections :: B.ByteString -> Maybe [SectionData]
parseSections sectionJson = do
    decoded <- decode sectionJson
    return $ getData decoded

getSectionStudents :: [SectionData] -> [[String]]
getSectionStudents sectionData = map (getStudents . getSection) sectionData
