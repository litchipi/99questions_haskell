
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

all_tests = [
	]
