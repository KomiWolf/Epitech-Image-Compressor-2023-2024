{-
-- EPITECH PROJECT, 2024
-- compressor
-- File description:
-- Main
-}

import System.Environment (getArgs)
import Data.Maybe (fromJust)
import FillContent
import MyData
import FillConf
import HandleError
import DoKMean

main :: IO ()
main = do
    args <- getArgs
    checkArgsNumber args
    checkArgs args
    let updated = getOpts defaultConf args
    fileContent <- readFile (toVar (file (fromJust updated)) "")
    let contents = fillContent (lines fileContent)
    checkContent contents
    doKMean (toVar updated defaultConf) contents
