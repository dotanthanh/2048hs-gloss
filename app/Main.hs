module Main(main) where

import Graphics.Gloss

window :: Display
window = InWindow "2048" (600, 600) (50, 50)

background :: Color
background = white

drawing :: Picture
drawing = circle 200

board :: Picture
board = pictures $ map (\x -> translate (x * 10) 0 $ circle 20)  [1,2..16]

main :: IO ()
main = display window background board
