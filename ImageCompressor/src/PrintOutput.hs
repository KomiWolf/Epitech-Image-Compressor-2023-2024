{-
-- EPITECH PROJECT, 2024
-- compressor
-- File description:
-- PrintOutput
-}

module PrintOutput (printOutput) where

import MyData

printColor :: (Int, Int, Int) -> IO ()
printColor (r, g, b) =
    putStrLn ("(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")")

printPosition :: (Int, Int) -> IO ()
printPosition (x, y) =
    putStr ("(" ++ show x ++ "," ++ show y ++ ") ")

printContent :: [Content] -> IO ()
printContent [] = return ()
printContent (actual:other) =
    printPosition (pos actual) >> printColor (rgb actual) >> printContent other

printOutput :: [Output] -> IO ()
printOutput [] = return ()
printOutput (actual:other) =
    putStrLn "--" >>
    printColor (color actual) >>
    putStrLn "-" >>
    printContent (content actual) >>
    printOutput other
