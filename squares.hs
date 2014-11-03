
main = do {
			putStrLn $ 
			"Squares: "
			++ "\n" ++
			(printSeparateLists (computeSquareSets 1 100))
}

--Takes a list of lists
----Prints different lists on different lines
printSeparateLists :: [[Int]] -> String
printSeparateLists (x:xs) = show x ++ "    =    " ++ show (computeSum x) ++ "\n" ++ printSeparateLists xs
printSeparateLists [] = show "----------------------------------------------------------------------------"

--Take the sum of a list of Ints
computeSum :: [Int] -> Int
computeSum (x:xs) = x + computeSum xs
computeSum [] = 0

--Compute squares in sets of 10 within the range specified
computeSquareSets :: Int -> Int -> [[Int]]
computeSquareSets beg end = if beg < end 
								then (computeSquares [beg..(beg + 9)]) : (computeSquareSets (beg + 10) end)
							else []
	--computeSquares [21..30]
	--computeSquares [31..40]
	--computeSquares [41..50]
	--computeSquares [51..60]
	--computeSquares [61..70]
	--computeSquares [71..80]
	--computeSquares [81..90]
	--computeSquares [91..100]



--Takes a list of integers, computes the squares for them all
computeSquares :: [Int] -> [Int]
computeSquares myInts = map (\ x -> x * x) myInts