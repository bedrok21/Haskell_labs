-- Вилучити зі списку елементи у позиціях, 
-- що відповідають квадратам цілих чисел

listSquares :: [Int]
listSquares = map (\x -> x*x) [1..]

removeAtSquares :: [a] -> [a]
removeAtSquares xs = [x | (x, i) <- zip xs [1..], i `notElem` take i listSquares]

main :: IO ()
main = do
  inputData <- readFile "input.txt"
  print (removeAtSquares (map read (words inputData) :: [Int]))

  