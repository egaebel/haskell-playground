main = do {
    putStrLn (
        "Merge Lists tests..."
        ++ "\n" ++ 
        show (mergeLists' [1, 3, 5, 7, 9, 11, 13] [2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 9999]) 
        ++ "\n" ++ 
        show (mergeLists' [] [17, 84])
        ++ "\n" ++ 
        show (mergeLists' [22, 99] [])
        ++ "\n" ++ 
        show (mergeLists' ([]) ([]::[Int]))
        ++ "\n" ++ 
        "Even Indices:"
        ++ "\n" ++ 
        show (evenIndices' [0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
        ++ "\n" ++ 
        "Odd Indices:"
        ++ "\n" ++ 
        show (oddIndices' [0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
        ++ "\n" ++ 
        "Now for mergeSort"
        ++ "\n" ++
        show (mergeSort' [7, 3, 17]::[Int])
        ++ "\n" ++
        show (mergeSort' [7, 3, 17, 89, 455, 1, 2, 3, 65, 17, 89, 100, 111, 954, 444, 12, 3, 5, 67, 17]::[Int])
        ++ "\n" ++
        show (mergeSort' []::[Int])
        ++ "\n" ++
        show (mergeSort' [-7, -8, -9, -99, -2, -35, -64, -0, 7, 2, 3, 11, 23, 98, -99, -999, -76, 32, 3, 4, 5, -1]::[Int])
    )
}

mergeSort' :: (Ord a) => [a] -> [a]
mergeSort' [] = []
mergeSort' (x:xs) = 
    if xs == [] 
        then [x] 
    else
        let 
            evenList = (evenIndices' (x:xs))
            oddList = (oddIndices' (x:xs))
        in (mergeLists' (mergeSort' oddList) (mergeSort' evenList))

indicesHelper :: (Ord a) => [a] -> [a]
indicesHelper [] = []
indicesHelper (x:xs) = if xs == [] then [x] else x:(indicesHelper (tail xs))

oddIndices' :: (Ord a) => [a] -> [a]
oddIndices' theList = (indicesHelper (tail theList))

evenIndices' :: (Ord a) => [a] -> [a]
evenIndices' theList = (indicesHelper theList)

mergeLists' :: (Ord a) => [a] -> [a] -> [a]
mergeLists' a [] = a
mergeLists' [] b = b
mergeLists' (x:xs) (y:ys)
    | x <= y = x:mergeLists' xs (y:ys)
    | otherwise = y:mergeLists' (x:xs) ys
