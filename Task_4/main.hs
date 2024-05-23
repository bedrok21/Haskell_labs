import System.IO (readFile)
import Data.List (span)
import Text.Read (readMaybe)


type Production = (String, [String])
type Grammar = [Production]

demoGrammar :: Grammar
demoGrammar = [ ("S", ["a", "A"]),
            ("A", ["b", "B", "x"]),
            ("B", ["c", "d", "B"]),
            ("B", ["t", "d"]),
            ("B", []) ]

data ParseTree = Node String [ParseTree] | Leaf String deriving (Show)


expand :: Grammar -> String -> [[String]]
expand grammar sym = [ symbols | (s, symbols) <- grammar, s == sym ]


buildParseTrees :: Grammar -> String -> Int -> [ParseTree]
buildParseTrees grammar start maxDepth = go start 0
  where
    go sym depth
      | depth > maxDepth = [Leaf sym]
      | otherwise = case expand grammar sym of
          [] -> [Leaf sym]
          expansions -> [Node sym subtrees | expansion <- expansions, subtrees <- sequence (map (`go` (depth + 1)) expansion)]
                  

extractResults :: ParseTree -> [String]
extractResults (Leaf sym) = [sym]
extractResults (Node _ subtrees) = map concat (sequence (map extractResults subtrees))


minLengthStrings :: [String] -> (Int, [String])
minLengthStrings xs
  | any (\s -> length s == minLength + 1 || length s == minLength + 1) xs = (-1, [])
  | otherwise = (minLength, filter (\s -> length s == minLength || length s == minLength +1) xs)
  where minLength = minimum (map length xs)


parseLine :: String -> (String, [String])
parseLine line =
  let (lhs, rhs) = span (/= '-') line
      rhsSymbols = words $ drop 3 rhs
  in (lhs, rhsSymbols)


readGrammar :: FilePath -> IO Grammar
readGrammar path = do
  content <- readFile path
  let linesOfGrammar = lines content
      grammar = map parseLine linesOfGrammar
  return grammar


main :: IO ()
main = do
  putStrLn "Введiть шлях до файлу з граматикою:"
  path <- getLine
  grammar <- readGrammar path
  putStrLn "Введiть глибину рекурсiї:"
  depth <- getLine
  putStrLn "Введiть нетермiнал:"
  nonterm <- getLine
  case readMaybe depth :: Maybe Int of
        Just depth -> do
            let trees = buildParseTrees grammar nonterm depth
                (minLen, results) = minLengthStrings (concatMap extractResults trees)
            -- print trees
            putStrLn(show (concatMap extractResults trees))
            if all (`notElem` map fst grammar) results 
                then do
                    putStrLn ("Мiнiмальна довжина:" ++ show minLen)
                    putStrLn "Слова з мiнiмальною довжиною:"
                    mapM_ putStrLn results
                else putStrLn "Слів з мiнiмальною довжиною не знайдено"
        Nothing -> putStrLn "Помилка: введене значення не є цiлим числом."
  
  