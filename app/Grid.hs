module Grid where

import Graphics.Gloss

type Value = Int

type Position = Int

data Grid = Grid Value Position Position

getPicture :: Grid -> Picture
getPicture (Grid n x y)
    | x < 0 || x > 3 || y < 0 || y > 3 = blank
    | otherwise = translate x' y' $ pictures [container, txt]
		where
			x' = fromIntegral $ x*50
			y' = fromIntegral $ y*50
			container = rectangleWire 50 50
			txt = translate (negate 10) (negate 10) $ scale 0.2 0.2 $ text (show n)

