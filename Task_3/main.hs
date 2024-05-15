import Data.List (nub)
import Data.Maybe (fromMaybe, mapMaybe)
import System.IO

-- Тип стану
type State = Int

-- Тип скінченого автомата
data DFA = DFA { states :: [State]
               , alphabet :: [Char]
               , transition :: State -> Char -> State
               , startState :: State
               , acceptStates :: [State]
               }

-- Читання автомата з файлу
readDFA :: FilePath -> IO DFA
readDFA path = do
  contents <- readFile path
  let ls = lines contents
      sts = parseStates $ head ls
      alpha = parseAlphabet $ ls !! 1
      start = parseStart $ ls !! 2
      accepts = parseAccept $ ls !! 3
      trans = parseTransitions $ drop 4 ls
  return $ DFA { states = sts
               , alphabet = alpha
               , transition = trans
               , startState = start
               , acceptStates = accepts
               }

parseStates :: String -> [State]
parseStates = map read . words . drop 8

parseAlphabet :: String -> [Char]
parseAlphabet = filter (/= ' ') . drop 10

parseStart :: String -> State
parseStart = read . drop 7

parseAccept :: String -> [State]
parseAccept = map read . words . drop 8

parseTransitions :: [String] -> State -> Char -> State
parseTransitions lines = \s c -> fromMaybe (-1) $ lookup (s, c) transList
  where
    transList = mapMaybe parseTransition lines
    parseTransition line = case words line of
                             [a, [b], c] -> Just ((read a, b), read c)
                             _ -> Nothing

acceptsSuffix :: DFA -> String -> (Bool, String)
acceptsSuffix dfa w = 
  let endState = foldl (transition dfa) (startState dfa) w
  in if endState == -1
     then (False, w) 
     else if endState `elem` acceptStates dfa
          then (True, w)
          else anySuffix endState w (alphabet dfa) []
  where
    anySuffix :: State -> String -> [Char] -> [State] -> (Bool, String)
    anySuffix _ _ [] _ = (False, "")
    anySuffix state prefix (x:xs) visited =
      let nextState = transition dfa state x
      in if nextState `elem` acceptStates dfa
         then (True, prefix ++ [x])
         else
           if nextState `elem` visited
              then anySuffix state prefix xs visited
              else
                let (found, suffix) = anySuffix nextState (prefix ++ [x]) (x:xs) (visited ++ [nextState])
                in if found
                   then (True, suffix)
                   else anySuffix state prefix xs visited


main :: IO ()
main = do
  dfa <- readDFA "automat.txt"
  putStrLn "Введiть слово w:"
  word <- getLine
  let (result, wy) = acceptsSuffix dfa word
  if result
    then putStrLn $ "Скiнченний автомат приймає слово виду wy. Приклад: " ++ wy
    else putStrLn $ "Скiнченний автомат не приймає слова виду wy."
