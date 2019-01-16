{-# LANGUAGE FlexibleContexts #-}

module Main(main) where

import System.Random
import Graphics.Gloss
import Grid
import Board
import Typeclass

window :: Display
window = InWindow "2048" (600, 600) (50, 50)

background :: Color
background = white

main :: IO ()
main = display window background $ render board
