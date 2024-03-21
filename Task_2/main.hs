-- Розбити заданий список на кілька списків, записуючи у перший список
-- значення, які зустрічаються у початковому списку лише один раз, у другий – які
-- зустрічаються двічі, у третій – тричі і т.д.


sortLT2 :: [(a, Int)] -> [(a, Int)]
sortLT2 [] = []
sortLT2 (x:xs) = sortLT2 [y | y <- xs, snd y < snd x]
                  ++ [x]
                  ++ sortLT2 [y | y <- xs, snd y >= snd x]


countOccurrences :: Eq a => [a] -> [(a, Int)]
countOccurrences [] = []
countOccurrences (x:xs) = (x, count x xs + 1) : countOccurrences (filter (/= x) xs)
  where
    count _ [] = 0
    count y (z:zs)
      | y == z    = 1 + count y zs
      | otherwise = count y zs


insertEmpty :: (a1, Int) -> [(a2, Int)] -> [[a3]]
insertEmpty y [] = replicate 0 []
insertEmpty y ys = replicate (abs (snd (head ys) - snd y) - 1) [] 


groupByFrequency :: [(a, Int)] -> [[a]]
groupByFrequency [] = []
groupByFrequency (x:xs) = (fst x : map fst (filter (\(_, y) -> y == snd x) xs))
                          : insertEmpty x remain
                          ++ groupByFrequency remain
  where
    remain = filter (\(_, y) -> y /= snd x) xs 


main :: IO ()
main = do
  inputData <- readFile "input.txt"
  let list = map read (words inputData) :: [Integer]
  let sortedTouple = sortLT2 (countOccurrences list)
  if null sortedTouple 
    then print ([] :: [Int])
    else print (insertEmpty (1,0) [head sortedTouple] ++ groupByFrequency sortedTouple)
