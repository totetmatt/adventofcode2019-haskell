main :: IO ()

isOrder [_] = True
isOrder (x:y:xs) | x <= y  = isOrder (y:xs)
                 | otherwise = False

adj2 [_] = False
adj2 (x:y:xs) | x == y = True
              | otherwise = adj2 (y:xs)            
part1 x = (isOrder x) && (adj2 x) 

only1adj2 [_] y = y
only1adj2 (x:y:xs) c  | x == y && (null c) = only1adj2 (y:xs) (2:[])
                      | x == y  = only1adj2 (y:xs) (((head c) + 1):(tail c))
                      | otherwise = only1adj2 (y:xs) (1:c)

part2 x = (isOrder x) && (elem 2 (only1adj2 x []))

countPart1 = length . filter (part1 . show)

countPart2 = length . filter (part2 . show)


main = do
        print (countPart1 [152085..670283])
        
        print (countPart2 [152085..670283])

