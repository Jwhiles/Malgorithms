main :: IO ()
main = print $ run "3 4 2 * 1 5 - 2 3 ^ ^ / +"

run :: String -> Int
run s = evaluate (words s) []


evaluate :: [String] -> [Int] -> Int
evaluate (x:xs) (f:f2:stack) = 
  case x of 
    "*" -> evaluate xs ((f2 * f):stack) 
    "/" -> evaluate xs ((f2 `div` f):stack) 
    "+" -> evaluate xs ((f2 + f):stack) 
    "-" -> evaluate xs ((f2 - f):stack) 
    "^" -> evaluate xs ((f2 ^ f):stack) 
    _ -> evaluate xs ((read x :: Int):f:f2:stack) 
evaluate (x:xs) [] = 
  case x of 
    _ -> evaluate xs ((read x :: Int):[]) 
evaluate (x:xs) (f:stack) = 
  case x of 
    _ -> evaluate xs ((read x :: Int):f:stack) 
evaluate _ (f:stack) = f
