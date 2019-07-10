import qualified

Graphics.Gloss.Data.Point.Arithmetic as V

cK :: Int -> Point -> Vetor -> Point -> Picture
cK 0 p d sz = line [p, p v.+ mulSV sz d]
cK n p d sz = pictures [ cK (n-1) p d (sz/3),
                       cK (n-1) p1 (rotateV (pi/3) d) (sz/3),
                       cK (n-1) p2 (rotateV (3*pi/4) d (sz/3),
                       cK (n-1) p3 d (sz/3) ]
