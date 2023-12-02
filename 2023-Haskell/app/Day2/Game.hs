module Day2.Game where
    data Draw = Draw { red :: Int, green :: Int, blue :: Int} deriving (Show)
    data Game = Game { gameNo :: Int, draws :: [Draw] } deriving (Show)