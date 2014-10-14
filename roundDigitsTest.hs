main = do {
	putStrLn $ show $ roundDigits 233333333.3232337 3
}

roundDigits :: Double -> Int -> Double
roundDigits floater digits = (fromInteger $ round $ floater * (10^digits)) / (10.0^^digits)