module Main where

import Pong

main :: IO ()
main = do
    _ <- playGame
    putStrLn "Thanks for playing Pong!"