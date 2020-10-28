import Control.Monad
import Data.Maybe (fromJust)
import Text.Read (readMaybe)


readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

numbers = ["1", "2", "3", "24", "4"]

testA = forM numbers readMaybeInt
testB = forM_ numbers putStrLn
