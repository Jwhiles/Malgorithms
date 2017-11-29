import           Data.Char   (isNumber)
import           Data.List   (intersperse)
import           Data.Maybe  (fromMaybe)
import           Data.Monoid


main :: IO ()
main = print $ run "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"

run input = mconcat . intersperse " " $ shunt (words input) [] []

type Input = [String]
type Stack = [String]
type Result = [String]

shunt :: Input -> Stack -> Result -> Result
shunt (x:xs) (y:ys) r = handleInputAndStack (x:xs) (y:ys) r
shunt (x:xs) _ r      = handleInput (x:xs) r
shunt _ (y:ys) r      = handleStack (y:ys) r
shunt _ _ r           = r

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

handleInputAndStack :: Input -> Stack -> Result -> Result
handleInputAndStack (x:xs) (y:ys) r
    | isNumber (head x) = shunt xs (y:ys) (r <> [x])
    | x == ")" = handleRightParen xs (y:ys) r
    | y == "(" = shunt xs (x:y:ys) r
    | comparePrecedence x y == GT = shunt xs (x:y:ys) r
    | comparePrecedence x y == EQ && x == "^" = shunt xs (x:y:ys) r
    | comparePrecedence x y == EQ = shunt (x:xs) ys (r <> [y])
    | otherwise = shunt (x:xs) ys (r <> [y])

handleInput :: Input -> Result -> Result
handleInput (x:xs) r
    | isNumber (head x) = shunt xs [] (r <> [x])
    | otherwise = shunt xs [x] r

handleStack :: Stack -> Result -> Result
handleStack (x:xs) r = shunt [] xs (r <> [x])


