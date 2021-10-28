
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

all_tests = [
	tests_solution_1,
	tests_solution_2,
	tests_solution_3,
	tests_solution_4,
	tests_solution_5,
	tests_solution_6
	]
