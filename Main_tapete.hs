
module Main(main) where

import Graphics.Gloss

width, height, offset :: Int
width = 500
height = 500
offset = -20

window :: Display
window = InWindow "fractal quadrado" (width, height) (offset, offset)

background :: Color
background = white

sp:: Int -> Point -> Float -> Picture
sp 0 _ _ = blank
sp n (x, y) sz = pictures [ polygon [(x+sz, y+sz), (x+sz, y+2*sz), (x+2*sz, y+2*sz), (x+2*sz, y+sz)]
                  ,sp (n-1) (x, y) (sz/3)
                  ,sp (n-1) (x+sz, y) (sz/3)
                  ,sp (n-1) (x+2*sz, y) (sz/3)
                  ,sp (n-1) (x, y+sz) (sz/3)
                  ,sp (n-1) (x+2*sz, y+sz) (sz/3)  
                  ,sp (n-1) (x, y+2*sz) (sz/3) 
                  ,sp (n-1) (x+sz, y+2*sz) (sz/3) 
                  ,sp (n-1) (x+2*sz, y+2*sz) (sz/3) ]
                            
main :: IO ()
main = display window background (sp 4 (-150, -150) 200)


