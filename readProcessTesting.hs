module Main where
import System.Process

main = do
	putStrLn $ "taking that md5sum...ohhh yeahh of readProcessTesting.hs"
	let output = runMd5sum
	putStrLn $ "md5 output: " ++ output

runMd5sum :: IO (String)
runMd5sum = do
	output <- readProcess "md5sum" ["readProcessTesting.hs"] ""
	return output