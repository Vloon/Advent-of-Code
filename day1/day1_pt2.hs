-- Sorts a list
insertionsort :: Ord a => [a] -> [a]
insertionsort xs = _insertionsort xs []
    where
    _insertionsort :: Ord a => [a] -> [a] -> [a]
    _insertionsort [] as        = as
    _insertionsort (x:xs) as    = _insertionsort xs (insert as x)

    insert :: Ord a => [a] -> a -> [a]
    insert [] x     = [x]
    insert (a:as) x 
        | x < a     = x:a:as
        | otherwise = a:insert as x

-- Calculates the similarity between two lists 
similarity :: [Int] -> [Int] -> Int
similarity left right = _similarity sleft sright first 0 0 
    where 
    sleft = insertionsort left
    sright = insertionsort right
    first = (sleft!!0) - 1 -- 1 less than the first sorted element will not exist in the list. 
    
    -- Calculates the similarity by assuming a sorted list, and keeping track of the previous number and similarity score, so you only have to count each number once..
    _similarity :: [Int] -> [Int] -> Int -> Int -> Int -> Int
    _similarity [] _ _ _ acc    = acc
    _similarity (l:ls) rs prev_num prev_sim acc -- This line needs to get priority: if we have the same number multiple times in left, even though right is empty we should still count them.
        | l == prev_num         = _similarity ls        rs      prev_num    prev_sim    (acc + prev_sim) 
    _similarity _ [] _ _ acc    = acc
    _similarity (l:ls) (r:rs) prev_num prev_sim acc 
        | l == r                = _similarity ls        rs      l           sim         (acc + sim)
        | l > r                 = _similarity (l:ls)    rs      prev_num    prev_sim    acc
        | otherwise             = _similarity ls        (r:rs)  prev_num    prev_sim    acc
        where 
            num = length (filter (== l)  (r:rs))
            sim = l * num

main = do
    -- Read input file
    contents <- readFile "input.txt"
    let singlewords = words contents -- Split by space 
    let input_list = [read s | s <- singlewords] -- Cast to Int

    let len = length input_list
    let left = [input_list!!i | i <- [0,2..len-1]]
    let right = [input_list!!i | i <- [1,3..len-1]]

    let total_similarity = similarity left right
    print(total_similarity)