ouEx1:: Bool->Bool->Bool
ouEx1 p q
	| p && q = False
	| p && not q = True
	| not p && q = True
	| otherwise = False


ouEx2:: Bool->Bool->Bool
ouEx2 p q
	| p && q = False
	| not p && not q = False
	| otherwise = True