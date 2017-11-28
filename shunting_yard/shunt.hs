import           Data.Char   (isNumber)
import           Data.List   (intersperse)
import           Data.Maybe  (fromMaybe)
import           Data.Monoid

type Input = [String]
type Stack = [String]
type Result = [String]

operatorMap :: [(String, Int)]
operatorMap = [("(", 10), ("^", 4), ("*", 3), ("/", 3), ("+", 2), ("-", 2)]

comparePrecedence :: String -> String -> Ordering
comparePrecedence c1 c2 =
    let p1 = lookup c1 operatorMap
        p2 = lookup c2 operatorMap
    in compare (fromMaybe 0 p1) (fromMaybe 0 p2)

handleRightParen :: Input -> Stack -> Result -> Result
handleRightParen input (first:rest) result =
    case first of
        "(" -> shunt input rest result
        _   -> handleRightParen input rest (result <> [first])
handleRightParen input _ result = shunt input [] result

shunt :: Input -> Stack -> Result -> Result
shunt (x:xs) (y:ys) r 
    | isNumber (head x) = shunt xs (y:ys) (r <> [x])
    | x == ")" = handleRightParen xs (y:ys) r
    | y == "(" = shunt xs (x:y:ys) r
    | comparePrecedence x y == GT = shunt xs (x:y:ys) r
    | comparePrecedence x y == EQ && x == "^" = shunt xs (x:y:ys) r
    | comparePrecedence x y == EQ = shunt (x:xs) ys (r <> [y])
    | otherwise = shunt (x:xs) ys (r <> [y])
shunt (x:xs) _ r
    | isNumber (head x) = shunt xs [] (r <> [x])
    | otherwise = shunt xs [x] r
shunt _ (y:ys) r = shunt [] ys (r <> [y])
shunt _ _ r = r

run input = mconcat . intersperse " " $ shunt (words input) [] []

main :: IO ()
main = print $ run "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"
