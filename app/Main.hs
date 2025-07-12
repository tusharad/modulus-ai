module Main (main) where

import App.Core
import App.DB

main :: IO ()
main = do 
    withDatabase "chat.db" $ \conn ->
        createTables conn
    runApp
