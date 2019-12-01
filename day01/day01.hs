import System.Environment
main :: IO ()

f :: Int -> Int
f m  = let mass = fromIntegral(m)
          in max 0 (floor ( mass / 3) - 2)

toInt :: String -> Int
toInt i = read i :: Int


fullMass 0 = 0
fullMass i = (f i) + (fullMass (f i))


fMass = f . toInt
ffMass = fullMass . toInt

main = do 
        s <- readFile "input01"
        let l = lines s
        print (sum (map fMass l))
        print (sum (map ffMass l))