main = run

run :: IO (Int, Int, [(Int,Int,Int)])
run = do
  contents <- readFile "text.txt"   -- use '<-' here so that 'contents' is a String
  let [a,b,c] = lines contents      -- split on newlines
  let firstLine  = read (init a)    -- 'init' drops the trailing period
  let secondLine = read (init b)    
  let thirdLine  = read (init c)    -- this reads a list of Int-tuples
  return (firstLine, secondLine, thirdLine)