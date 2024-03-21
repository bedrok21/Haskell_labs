-- Знайти максимальний елемент та переставити 
-- всі його примірники в початок списку.

moveMaxToFront :: Ord a => [a] -> [a]
moveMaxToFront [] = []
moveMaxToFront xs = filter (== maximum xs) xs 
                    ++ filter (/= maximum xs) xs

main :: IO ()
main = do
  inputData <- readFile "input.txt"
  print (moveMaxToFront (map read (words inputData) :: [Int]))

