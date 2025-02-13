{-
-- EPITECH PROJECT, 2024
-- compressor
-- File description:
-- FillContent
-}

module FillContent (fillContent) where

import MyData (Content(..))

getColor :: String -> (Int, Int, Int)
getColor val = case reads val of
    [(color, "")] -> color
    _ -> (-1, -1, -1)

getPosition :: String -> (Int, Int)
getPosition val = case reads val of
    [(position, "")] -> position
    _ -> (-1, -1)

fillContent :: [String] -> [Content]
fillContent array = map processLine array
    where
        processLine line =
            let (posStr, colorStr) = break (== ' ') line
                position = getPosition posStr
                color = getColor colorStr
            in Content position color