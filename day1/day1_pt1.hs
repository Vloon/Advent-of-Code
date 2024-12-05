import System.IO
import Control.Monad

-- Returns the absolute difference between two numbers
difference :: Integer -> Integer -> Integer
difference a b
    | a < b     = b - a
    | otherwise = a - b

-- Sorts a list
insertionsort :: (Ord a) => [a] -> [a]
insertionsort xs = _insertionsort xs []
    where
    _insertionsort :: (Ord a) => [a] -> [a] -> [a]
    _insertionsort [] as        = as
    _insertionsort (x:xs) as    = _insertionsort xs (insert as x)

    insert :: (Ord a) => [a] -> a -> [a]
    insert [] x     = [x]
    insert (a:as) x 
        | x < a     = x:a:as
        | otherwise = a:insert as x

-- Pairs a list of number into tuples of two
pair_up :: (Ord a) => [a] -> [(a,a)]
pair_up input_list = zip left_column right_column
    where -- Turn list into two columns
    len = length input_list
    left_column = insertionsort [input_list!!i | i <- [0,2..len-1]]
    right_column = insertionsort [input_list!!i | i <- [1,3..len-1]]

main = do
    -- Read input file
    contents <- readFile "input.txt"
    let singlewords = words contents -- Split string by space 
    let input_list = [read s :: Integer | s <- singlewords] -- Cast to int

    -- -- Pair each item up
    let paired_list = pair_up input_list

    -- Calculate the difference per pair
    let differences = [difference left right | (left, right) <- paired_list]

    -- Calculate the sum
    let total_difference = sum differences
    print(total_difference)