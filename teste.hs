pushRight :: String -> String
pushRight str = x++str  
  where d = (lineLength - (length str))
  comp = [x |" " <- [1..d]]
 

pushRight :: String -> Int -> String
pushRight str lineLength = x++str 
  where d = (lineLength - (length str))
  comp = [x |" " <- [1..d]]
          
		 

