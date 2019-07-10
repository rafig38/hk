-- import qualified

module Main(main) where

import Graphics.Gloss

window :: Display
window = FullScreen
-- window = InWindow "fractal" (300, 300) (30, 50)

background :: Color
background = white

-- ex1 = pictures [color black (circleSolid 100)]
ex1 = pictures [color black (rectangleSolid 50 70)]
--drawing :: Picture
--drawing = rectangleSolid 50 70
--ex1 :: Int -> Float -> Float -> Picture
--ex1 0 l c = blank
--ex1 n l c = pictures [ color black( ex1 n l c, ex1 (n-1) (l/2) (c/2)) ]

main :: IO ()
main = display window background ex1


