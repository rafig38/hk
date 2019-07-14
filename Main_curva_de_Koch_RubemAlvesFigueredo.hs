module Main(main) where
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss.Data.Vector

window :: Display
window = InWindow "Curva de Koch" (500, 500) (-20, -20)

ckoch:: Int -> Path-> Picture
ckoch 0 ts = line ts
ckoch n [p,q] = pictures[ckoch (n-1) [p, p1]
                        , ckoch (n-1) [p1, p2]
                        , ckoch (n-1) [p2, p3]
                        , ckoch (n-1) [p3, q] ]
                        where
                            v = mulSV (1/3) (q V.- p)
                            p1 = p V.+ v
                            p2 = p1 V.+ rotateV (pi/3) v
                            p3 = p2 V.+ rotateV (-pi/3) v

main :: IO ()
main = display window white (ckoch 3 [(-100, 10),(300, 10)])
