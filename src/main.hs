
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
	tests_solution_11
	]
