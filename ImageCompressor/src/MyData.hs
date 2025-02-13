{-
-- EPITECH PROJECT, 2024
-- compressor
-- File description:
-- Lib
-}

module MyData (Conf(..), Content(..), Output(..), toVar) where

data Conf = Conf {
    nColor :: Maybe Int,
    convergence :: Maybe Float,
    file :: Maybe String
} deriving (Show)

data Content = Content {
    pos :: (Int, Int),
    rgb :: (Int, Int, Int)
} deriving (Show)

data Output = Output {
    color :: (Int, Int, Int),
    content :: [Content]
} deriving (Show)

instance Eq Output where
    (==) (Output color1 _) (Output color2 _) = color1 == color2

toVar :: Maybe a -> a -> a
toVar maybeValue defaultValue =
    case maybeValue of
        Just value -> value
        Nothing -> defaultValue
