-- soma de fatoriais



sumFat :: Int -> Int

sumFat 0 = 1

sunFat n = sunFat (n-1) + fatorial n

--==================================================
raizQI :: Int -> Int

raizQI 0 = 0

  | raizQI n
   (r+1)¨2 == n = r+1
    
  | otherwise=r
    
              where r = raizQI (n-1)   

--==================================================

type Person = String


type Book = String



type Database = [ (Person, Book) ]



exampleBase :: Database


exampleBase = [("Alice", "Tintin"), ("Anna", "Little women"),("Alice", "Asterix"), ("Rory", "Tintin")]



--books :: Database -> Person -> [Book]


--borrowers :: Database -> Book -> [Person]


borrowed :: Database -> Book -> Bool


--numBorrowed :: Database -> Person -> Int



makeLoan :: Database -> Person -> Book -> Database


--returnLoan :: Database -> Person -> Book -> Database


-- =======================================================

numBorrowed :: Database -> Person -> Int

numBorrowed [] _ = 0

numBorrowed ((p1, lv):es) p
  
  | p == p1 = 1 + numBorrowed  es p
  
  | otherwise = numBorrowed  es p



-- =======================================================
books :: Database -> Person -> [Book]

books [] p = []

books ((p1, lv):es) p
  
  | p == p1 = lv: books es p
  
  | otherwise = books es p
-- ======================================================
borrowers :: Database -> Book -> [Person]

borrowers [] lv = []

borrowers ((p, lv1):es) lv
  
  | lv1 == lv = p: borrowers es lv
  
  | otherwise = borrowers es lv

--=======================================================


returnLoan :: Database -> Person -> Book -> Database

returnLoan [] p lv = []

returnLoan (p2, lv2):es) p lv
  
  | p==p2 && lv==lv2 = (p,lv):returnoLoan es p lv
  
  | otherwise = returnoLoan es p lv
--=======================================================