-- Turns ([1,2,3,4]) into pairs ([(1,2),(2,3),(3,4)])
make_pairs :: [a] -> [(a,a)]
make_pairs []       = []
make_pairs (x:xs)   = zip (x:xs) xs

-- Gives the absolute differece 
diff :: (Num a, Ord a) => a -> a -> a
diff a b 
    | a < b     = b - a
    | otherwise = a - b

-- Checks whether the given predicate holds for all successive pairs
pair_predicate_check :: (Ord a) => [a] -> (a -> a -> Bool) -> Bool
pair_predicate_check xs f = all (\(x,y) -> f x y) (make_pairs xs)

-- Check whether a list is strictly increasing
strictly_increasing :: (Ord a) => [a] -> Bool
strictly_increasing xs = pair_predicate_check xs (<)

-- Check whether a list is strictly decreasing
strictly_decreasing :: (Ord a) => [a] -> Bool
strictly_decreasing xs = pair_predicate_check xs (>)

-- Checks whether a report has at a jump of at most max_diff
difference_at_most :: (Num a, Ord a) => [a] -> a -> Bool
difference_at_most xs max_diff = pair_predicate_check xs (\x y -> diff x y <= max_diff)

-- Checks whether a report is valid
report_is_valid :: (Num a, Ord a) => [a] -> Bool
report_is_valid xs = ((strictly_decreasing xs) || (strictly_increasing xs)) && (difference_at_most xs 3)

-- Remove an element from list at index i
remove_elem :: (Eq a) => [a] -> Int -> [a]
remove_elem xs i 
    | i >= length xs    = error "Index i >= length of list xs: cannot remove element"
    | otherwise         = fst pair ++ (drop 1 (snd pair))
    where 
        pair = splitAt i xs

-- Check if report is valid with a single bad level removed.
valid_with_removal :: (Num a, Ord a) => [a] -> Bool
valid_with_removal xs = _valid_with_removal xs 0 
    where 
    _valid_with_removal :: (Num a, Ord a) => [a] -> Int -> Bool
    _valid_with_removal xs i 
        | i >= length xs                        = False
        | report_is_valid (remove_elem xs i)    = True
        | otherwise                             = _valid_with_removal xs (i+1) 

main = do
    -- Read input file
    content <- readFile "input.txt"
    let all_str_reports = lines content -- Split by lines
    let reports = [[read str_level :: Int | str_level <- str_report] | str_report <- [words str_report_ln  | str_report_ln <- all_str_reports]] -- Yes yes baby. It's a nested list comprehension.
    let valid_reports = [report | report <- reports, report_is_valid report] 
    let invalid_reports = [report | report <- reports, not (elem report valid_reports)]
    let valid_removed_reports = [report | report <- invalid_reports, valid_with_removal report] -- Check which reports are valid with 1 removal
    let total_valid_reports = length valid_reports + length valid_removed_reports
    print(total_valid_reports)