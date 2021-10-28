
import qualified Data.Maybe as Maybe
import qualified Debug.Trace as Debug

main :: IO ()
main = putStrLn $ perform_question 0

nb_questions = 99

perform_question :: Int -> String
perform_question nb | nb == nb_questions = "Congratulations ! You finished"
perform_question nb = result
	where
		valres = validate_solution nb
		result = if valres == Nothing
			  then "\nNo more questions to validate"
			  else if Maybe.fromJust valres == False
				  then "Solution " ++ (show (nb + 1)) ++ " validation failed"
				  else Debug.trace ("Question " ++ (show (nb + 1)) ++ " validated") perform_question $ nb + 1

validate_solution :: Int -> Maybe Bool
validate_solution nb | nb >= (length all_tests) = Nothing
validate_solution nb = Just $ all (== True) $ (all_tests !! nb)

-- Question 1
tests_solution_1 = [
	(solution_1 [1, 2, 3, 4]) == 4,
	(solution_1 ['x', 'y', 'z']) == 'z'
	]
solution_1 = last

-- Question 2
tests_solution_2 = [
	(solution_2 [1, 2, 3, 4]) == 3,
	(solution_2 ['a' .. 'z']) == 'y'
	]
solution_2 = last . init

-- Question 3
tests_solution_3 = [
	(solution_3 [1, 2, 3] 2) == 2,
	(solution_3 "haskell" 5) == 'e'
	]
solution_3 = \l x -> (l !! (x-1))

-- Question 4
tests_solution_4 = [
	(solution_4 [123, 456, 789]) == 3,
	(solution_4 "Hello, world!") == 13
	]
solution_4 = length

-- Question 5
tests_solution_5 = [
	(solution_5 "A man, a plan, a canal, panama!") == "!amanap ,lanac a ,nalp a ,nam A",
	(solution_5 [1, 2, 3, 4]) == [4, 3, 2, 1]
	]
solution_5 = reverse

-- Question 6
tests_solution_6 = [
	(solution_6 [1, 2, 3]) == False,
	(solution_6 "madamimadam") == True,
	(solution_6 [1, 2, 4, 8, 16, 8, 4, 2, 1]) == True
	]
solution_6 :: Eq a => [a] -> Bool
solution_6 l = (l == reverse l)

-- Question 7
data NestedList a = Elem a | List [NestedList a] deriving (Show)
tests_solution_7 = [
	(solution_7 (Elem 5)) == [5],
	(solution_7 (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])) == [1, 2, 3, 4, 5],
	(solution_7 (List [])) == ([] :: [Int])
	]

solution_7 :: Eq a => NestedList a -> [a]
solution_7 (Elem x) = [x]
solution_7 (List (x:xs)) = (solution_7 x) ++ (solution_7 (List xs))
solution_7 (List []) = []

-- Question 8
tests_solution_8 = [
	(solution_8 "aaaabccaadeeee") == "abcade",
	(solution_8 "taaaaatoooorgee") == "tatorge"
	]

solution_8 :: Eq a => [a] -> [a]
solution_8 [] = []
solution_8 (x:xs) = let (head, tail) = span (== x) xs in [x] ++ (solution_8 tail)

-- Question 9
tests_solution_9 = [
	(solution_9 ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']) == ["aaaa","b","cc","aa","d","eeee"]
	]

solution_9 :: Show a => Eq a => [a] -> [[a]]
solution_9 [] = []
solution_9 [x] = [[x]]
solution_9 (x:xs) = let (fst, snd) = span (== x) xs in [[x] ++ fst] ++ (solution_9 snd)

-- Question  10
tests_solution_10 = [
	(solution_10 "aaaabccaadeeee") == [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
	]

solution_10 :: Show a => Eq a => [a] -> [(Int, a)]
solution_10 [] = []
solution_10 [x] = [(1, x)]
solution_10 (x:xs) = let (fst, snd) = span (== x) xs in [((length fst)+1, x)] ++ (solution_10 snd)

-- Question 11

data LengthEncoded a = Single a | Multiple Int a deriving(Eq)
tests_solution_11 = [
	(solution_11 "aaaabccaadeeee") == [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']
	]

solution_11 :: Eq a => [a] -> [LengthEncoded a]
solution_11 [] = []
solution_11 [x] = [Single x]
solution_11 (x:xs) = let (fst, snd) = span (== x) xs in [encode fst] ++ (solution_11 snd)
	where
		encode [] = Single x
		encode [x] = Multiple 2 x
		encode (x:xs) = Multiple ((length xs)+2) x

-- Question 12
tests_solution_12 = [
	(solution_12 [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']) == "aaaabccaadeeee",
	(solution_12 [Multiple 2 'a',Multiple 3 'b',Single 'c',Single 'd', Multiple 5 'a']) == "aabbbcdaaaaa"
	]

solution_12 :: [LengthEncoded a] -> [a]
solution_12 [] = []
solution_12 [Single x] = [x]
solution_12 [Multiple n x] = take n (repeat x)
solution_12 (x:xs) = (solution_12 [x]) ++ (solution_12 xs)

-- Question 13
tests_solution_13 = [
	(solution_13 "aaaabccaadeeee") == [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']
	]

solution_13 :: Show a => Eq a => [a] -> [LengthEncoded a]
solution_13 l = encode $ foldr helper [] l
	where
		helper x [] = [(1, x)]
		helper x (y@(nb, yx):ys) = if yx == x
								then [(nb + 1, yx)] ++ ys
								else [(1, x), (nb, yx)] ++ ys
		encode [] = []
		encode [(nb, x)] = if nb == 1
						then [Single x]
						else [Multiple nb x]
		encode (x:xs) = encode [x] ++ encode xs

-- Question 14
tests_solution_14 = [
	(solution_14 [1, 2, 3]) == [1,1,2,2,3,3]
	]

solution_14 :: [a] -> [a]
solution_14 [] = []
solution_14 (x:xs) = [x, x] ++ solution_14 xs

-- Question 15
tests_solution_15 = [
	(solution_15 "abc" 3) == "aaabbbccc",
	(solution_15 "abc" 1) == "abc",
	(solution_15 "abc" 0) == ""
	]

solution_15 :: [a] -> Int -> [a]
solution_15 [] _ = []
solution_15 l 0 = []
solution_15 l 1 = l
solution_15 (x:xs) nb = (take nb (repeat x)) ++ solution_15 xs nb

-- Question 16
tests_solution_16 = [
	(solution_16 "abcdefghik" 3) == "abdeghk"
	]

solution_16 :: Show a => [a] -> Int -> [a]
solution_16 _ 0 = undefined
solution_16 _ 1 = []
solution_16 l n = let (res, _) = foldl (dropevery n) ([], 0) l in res
	where
		dropevery n (res, count) x = if (count + 1) >= n
							  then (res, 0)
							  else (res ++ [x], count + 1)

-- Question 17
tests_solution_17 = [
	(solution_17 "abcdefghik" 3) == ("abc", "defghik")
	]

solution_17 :: [a] -> Int -> ([a], [a])
solution_17 l 0 = (l, [])
solution_17 l nb = (take nb l, drop nb l)

-- Question 18
tests_solution_18 = [
	(solution_18 ['a','b','c','d','e','f','g','h','i','k'] 3 7) == "cdefg"
	]

solution_18 :: [a] -> Int -> Int -> [a]
solution_18 l indstart end = let start = indstart - 1 in take (end - start) (drop start l)

-- Question 19
tests_solution_19 = [
	(solution_19 ['a','b','c','d','e','f','g','h'] 3) == "defghabc",
	(solution_19 ['a','b','c','d','e','f','g','h'] (-2)) == "ghabcdef"
	]

-- Very VERY elegant solution
solution_19 xs n = take (length xs) $ drop (length xs + n) $ cycle xs
-- My solution:
-- solution_19 :: [a] -> Int -> [a]
-- solution_19 l 0 = l
-- solution_19 l nb | nb < 0 = solution_19 l ((length l) + nb)
-- solution_19 l nb = (drop nb l) ++ (take nb l)

-- Question 20
tests_solution_20 = [
	(solution_20 2 "abcd") == ('b', "acd"),
	(solution_20 5 "ablakzerjlcd") == ('k', "ablazerjlcd")
	]

solution_20 :: Int -> [a] -> (a, [a])
solution_20 k l = (l !! (k-1), (take (k-1) l) ++ (drop k l))

all_tests = [
	tests_solution_1,
	tests_solution_2,
	tests_solution_3,
	tests_solution_4,
	tests_solution_5,
	tests_solution_6,
	tests_solution_7,
	tests_solution_8,
	tests_solution_9,
	tests_solution_10,
	tests_solution_11,
	tests_solution_12,
	tests_solution_13,
	tests_solution_14,
	tests_solution_15,
	tests_solution_16,
	tests_solution_17,
	tests_solution_18,
	tests_solution_19,
	tests_solution_20
	]
