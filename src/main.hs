
import qualified System.Random as Random
import qualified Data.Maybe as Maybe
import qualified Debug.Trace as Debug
import qualified Data.List as List

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
solution_17 l nb = splitAt nb l

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
solution_20 k l = let (fst, snd) = splitAt (k-1) l in (l !! (k-1), fst ++ (tail snd))

-- Question 21
tests_solution_21 = [
	(solution_21 'X' "abcd" 2) == "aXbcd"
	]

solution_21 :: a -> [a] -> Int -> [a]
solution_21 char str 0 = undefined
solution_21 char str 1 = [char] ++ str
solution_21 char str ind = let (fst, snd) = splitAt (ind-1) str in fst ++ [char] ++ snd

-- Question 22
tests_solution_22 = [
	(solution_22 4 9) == [4, 5, 6, 7, 8, 9],
	(solution_22 2 4) == [2, 3, 4]
	]

solution_22 :: Int -> Int -> [Int]
solution_22 a b = [a..b]

-- Question 23
random_generator = Random.mkStdGen 1344353453234
solution_23_subset = "abcadefgh"
(solution_23_sample, newgen) = solution_23 solution_23_subset 3 random_generator
(solution_23_sample_2, _) = solution_23 solution_23_subset 3 newgen

tests_solution_23 = [
	(length solution_23_sample) == 3,
	(length solution_23_sample_2) == 3,
	foldr (\x acc -> acc && x `elem` solution_23_sample_2) True solution_23_sample_2,
	foldr (\x acc -> acc && x `elem` solution_23_subset) True solution_23_sample,
	solution_23_sample /= solution_23_sample_2
	]

solution_23 :: [a] -> Int -> Random.StdGen -> ([a], Random.StdGen)
solution_23 _ 0 g = ([], g)
solution_23 subset nb gen = let (rest, lastgen) = solution_23 new_subset (nb-1) newgen in ([picked] ++ rest, lastgen)
	where
		pop_el :: Int -> [a] -> (a, [a])
		pop_el ind list = let (fst, snd) = splitAt ind list in (list !! ind, fst ++ (tail snd))
		(rand_ind, newgen) = Random.randomR (0, (length subset)-1) gen
		(picked, new_subset) = pop_el rand_ind subset

-- Question 24
solution_24_sample = solution_24 150 155 $ Random.mkStdGen 3242342342
solution_24_sample_2 = solution_24 150 155 $ Random.mkStdGen 423445436
tests_solution_24 = [
	(length solution_24_sample) == 150,
	(maximum solution_24_sample_2) <= 155,
	solution_24_sample /= solution_24_sample_2
	]

solution_24 :: Random.RandomGen g => Int -> Int -> (g -> [Int])
solution_24 nb max | nb > max = error "Have to pick unique numbers, pick less or increase diversity"
solution_24 nb max = take nb . List.nub . Random.randomRs (1, max)

-- Question 25
testlist25 = "abcdefghijklmnop"
solution_25_sample = solution_25 testlist25 $ Random.mkStdGen 65423434
tests_solution_25 = [
	(length solution_25_sample) == (length testlist25),
	foldr (\el acc -> acc && (el `elem` testlist25)) True solution_25_sample,
	solution_25_sample /= testlist25
	]

solution_25 :: [a] -> (Random.StdGen -> [a])
solution_25 xs = solution_23_modified
	where
		solution_23_modified gen = let (res, _) = solution_23 xs (length xs) gen in res

-- Question 26
test_solution_26 :: Eq a => [[a]] -> [[a]] -> Bool
test_solution_26 exp got = lengthOK && elemsOK
	where
		lengthOK = (length got) == (length exp)
		elemsOK = foldr (\el acc -> acc && (el `elem` exp)) True $ got

allcombinations_1 = ["abc", "abd", "acd", "bcd"]
allcombinations_2 = [
		"ABCDE", "ABCDF", "ABCDG", "ABCDH", "ABCEF", "ABCEG", "ABCEH", "ABCFG", "ABCFH", "ABCGH", "ABDEF", "ABDEG",
		"ABDEH", "ABDFG", "ABDFH", "ABDGH", "ABEFG", "ABEFH", "ABEGH", "ABFGH", "ACDEF", "ACDEG", "ACDEH", "ACDFG",
		"ACDFH", "ACDGH", "ACEFG", "ACEFH", "ACEGH", "ACFGH", "ADEFG", "ADEFH", "ADEGH", "ADFGH", "AEFGH", "BCDEF",
		"BCDEG", "BCDEH", "BCDFG", "BCDFH", "BCDGH", "BCEFG", "BCEFH", "BCEGH", "BCFGH", "BDEFG", "BDEFH", "BDEGH",
		"BDFGH", "BEFGH", "CDEFG", "CDEFH", "CDEGH", "CDFGH", "CEFGH", "DEFGH"]

tests_solution_26 = [
	test_solution_26 allcombinations_1 $ solution_26 3 "abcd" [],
	test_solution_26 allcombinations_2 $ solution_26 5 "ABCDEFGH" []
	]

solution_26 :: Show a => Int -> [a] -> [[a]] -> [[a]]

solution_26 1 _ [] = []
solution_26 1 (x:[]) (p:px) = addel (p:px) x ++ (solution_26 1 [x] px)
	where
		addel :: [[a]] -> a -> [[a]]
		addel (p:px) x = [p ++ [x]] ++ addel px x
		addel [] x = []
solution_26 1 (x:xs) (p:px) = (solution_26 1 [x] (p:px)) ++ (solution_26 1 xs (p:px))

solution_26 n (x:xs) [] = solution_26 (n-1) xs [[x]] ++ (solution_26 n xs [])
solution_26 n (x:xs) px = solution_26 (n-1) xs (addprefix px x) ++ (solution_26 n xs px)
	where
		addprefix :: Show a => [[a]] -> a -> [[a]]
		addprefix [] el = []
		addprefix (p:px) el = [p ++ [el]] ++ addprefix px el
solution_26 n [] _ = []
solution_26 _ _ _ = undefined

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
	tests_solution_20,
	tests_solution_21,
	tests_solution_22,
	tests_solution_23,
	tests_solution_24,
	tests_solution_25,
	tests_solution_26
	]
