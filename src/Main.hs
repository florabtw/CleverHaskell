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

data Section = Section { getId :: String }
                       deriving (Show)

-- List of things that are dumb:
--              1. This
instance FromJSON Section where
    parseJSON (Object v) =
        Section <$> v .: "id"
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

main = undefined

server :: String
server = "https://api.clever.com/v1.1"

sections :: String
sections = server ++ "/sections"

studentsInSection :: String -> String
studentsInSection x = sections ++ "/" ++ x ++ "/students"

requestSections :: IO B.ByteString
requestSections = do
    request <- parseUrl sections
    let headers = [(fromString "Authorization", fromString "Bearer DEMO_TOKEN")]
        request' = request { requestHeaders = headers }
    res <- withManager $ httpLbs request'
    return $ responseBody res

parseSections :: B.ByteString -> Maybe [SectionData]
parseSections sectionJson = do
    decoded <- decode sectionJson
    return $ getData decoded

getSectionIds :: [SectionData] -> [String]
getSectionIds sectionData = map (getId . getSection) sectionData
