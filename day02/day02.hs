import System.Environment
import Data.Ix

main :: IO ()


toInt :: String -> Int
toInt i = read i :: Int

part2out = 19690720

intcode ptr op 
        | (op !! ptr) == 1  =  let nv = (op !! (op !! (ptr+1))) + (op !! (op !! (ptr+2)))
                                   nop = (insertAt (op !! (ptr+3) ) nv  op)
                               in  intcode (ptr + 4) nop
        | (op !! ptr) == 2  =  let nv = (op !! (op !! (ptr+1))) * (op !! (op !! (ptr+2)))
                                   nop = (insertAt (op !! (ptr+3) ) nv  op)
                               in  intcode (ptr + 4) nop
        | otherwise = op


setNoun n op  = insertAt 1 n op 
setVerb n op  = insertAt 2 n op

initIntcode noun verb = (setNoun noun) . (setVerb verb)

doIntcode noun verb op =  [(head (intcode 0 (initIntcode noun verb op))), noun, verb]

insertAt 0 el xs = el : (tail xs)
insertAt idx el xs = let (x,_:y) = splitAt idx xs
                     in  x ++ (el : y)
             
main = do 
        s <- readFile "input02"
        let l = head (lines s) -- Cheating to change input as "1 2 3" to use words
        let op = map toInt (words l)
        print (doIntcode 12 2 op)
        let [[r,rn,rv]] = [(doIntcode n v op) | n <- [0..100]
                     , v <- [0..100]
                     , (head (doIntcode n v op)) == part2out
                    ]
        print ((100 * rn) + rv)
