module Grid where

import Graphics.Gloss
import Typeclass

type Value = Int

type Position = (Int, Int)

data Grid = Grid Value Position
	deriving (Eq, Show)

-- left/right/up/down. Abbreviation is to avoid duplication with standard Left/Right
data Direction = L | R | U | D

instance Model Grid where
	render (Grid 0 _) = blank
	render (Grid n (x,y))
		| x < 0 || x > 3 || y < 0 || y > 3 = blank
		| otherwise = translate x' y' (pictures [container, txt])
			where
				x' = fromIntegral $ x*50
				y' = fromIntegral $ y*50
				container = rectangleWire 50 50
				txt = translate (negate 10) (negate 10) $ scale 0.2 0.2 $ text (show n)

combine :: Grid -> Grid -> (Grid, Maybe Grid)
combine (Grid 0 p1) (Grid n p2) = ((Grid n p1), Just (Grid 0 p2))
combine g g2@(Grid 0 _) = (g, Just g2)
combine g1@(Grid n1 p1) g2@(Grid n2 _)
	| n1 /= n2 = (g1, Just g2)
	| otherwise = (Grid (n1+n2) p1, Nothing)

move :: Direction -> Grid -> Grid
move L (Grid n (x,y)) = Grid n (x-1, y)
move R (Grid n (x,y)) =  Grid n (x+1, y)
move U (Grid n (x,y)) = Grid n (x, y+1)
move D (Grid n (x,y)) =  Grid n (x, y-1)
