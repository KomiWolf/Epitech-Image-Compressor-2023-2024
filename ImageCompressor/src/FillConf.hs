{-
-- EPITECH PROJECT, 2024
-- functionnal
-- File description:
-- FillConf
-}

module FillConf (defaultConf, getOpts) where

import Text.Read (readMaybe)
import MyData

defaultConf :: Conf
defaultConf = Conf {
    nColor = Nothing,
    convergence = Nothing,
    file = Nothing
}

getOpts :: Conf -> [String] -> Maybe Conf
getOpts conf [] = Just conf
getOpts conf (arg:val:other) = case arg of
    "-n" -> case readMaybe val of
        Just n -> getOpts (conf { nColor = Just n }) other
        Nothing -> Nothing
    "-l" -> case readMaybe val of
        Just l -> getOpts (conf { convergence = Just l }) other
        Nothing -> Nothing
    "-f" -> getOpts (conf { file = Just val }) other
    _ -> Nothing
getOpts _ _ = Nothing
