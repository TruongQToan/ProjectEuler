module Euler(euler1) where
modFilter :: Int -> Bool
modFilter x = mod x 3  == 0 || mod x 5 == 0
euler1 :: IO ()
euler1 = do 
    let r = sum $ filter modFilter $ [1..999]
    putStrLn $ "Result: " ++ show r

