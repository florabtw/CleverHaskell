module Main where

import Network.HTTP.Conduit
import Data.String (fromString)

main = getSections

server :: String
server = "https://api.clever.com/v1.1"

sections :: String
sections = server ++ "/sections"

studentsInSection :: String -> String
studentsInSection x = sections ++ "/" ++ x ++ "/students"

getSections = do
    request <- parseUrl sections
    let headers = [(fromString "Authorization", fromString "Bearer DEMO_TOKEN")]
        request' = request { requestHeaders = headers }
    res <- withManager $ httpLbs request'
    print res
