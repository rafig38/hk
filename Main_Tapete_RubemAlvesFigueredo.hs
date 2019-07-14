module Main(main) where
import Graphics.Gloss

window :: Display
window = InWindow "Tapete" (500, 500) (-20, -20)

background :: Color
background = white

tapete:: Int -> Point -> Float -> Picture
tapete 0 _ _ = blank
tapete n (x, y) sz = pictures [ polygon [(x+sz, y+sz), (x+sz, y+2*sz), (x+2*sz, y+2*sz), (x+2*sz, y+sz)]
                  ,tapete (n-1) (x, y) (sz/3)
                  ,tapete (n-1) (x+sz, y) (sz/3)
                  ,tapete (n-1) (x+2*sz, y) (sz/3)
                  ,tapete (n-1) (x, y+sz) (sz/3)
                  ,tapete (n-1) (x+2*sz, y+sz) (sz/3)
                  ,tapete (n-1) (x, y+2*sz) (sz/3)
                  ,tapete (n-1) (x+sz, y+2*sz) (sz/3)
                  ,tapete (n-1) (x+2*sz, y+2*sz) (sz/3) ]

main :: IO ()
main = display window background (tapete 4 (-150, -150) 200)

