{-# LANGUAGE OverloadedStrings #-}

module Haqu.Web where
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class (liftIO)
import Haqu.Model.Quiz
import Haqu.Model.Answer
import Haqu.FileReader
import Haqu.Components.Helper
import Haqu.Pages.Home
import Haqu.Pages.QuizPlayer
import Haqu.Pages.QuizQuestion
import Web.Scotty

type Html = String


main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev

  get  "/styles.css" styles
  get  "/" homeAction

  get "/quiz" quizAction

  get "/quiz/:id/start" (quizNameAction "get")
  post "/quiz/:id/start" (quizNameAction "post")

  get "/quiz/:id/result" resultAction

  get "/quiz/:quiz/:question" (questionAction "get")
  post "/quiz/:quiz/:question" (questionAction "post")


styles :: ActionM ()
styles = do
    setHeader "Content-Type" "text/css"
    file "static/styles.css"


resultAction :: ActionM ()
resultAction = do
  quizId <- captureParam "id"
  answers <- liftIO $ readQuizAnswers quizId
  quiz <- liftIO $ readQuizFile ("data/" ++ quizId ++ ".txt")
  htmlString $ htmlDoc
    ( e "H2" ("Results: " ++ q_name quiz)
    ++ e "P" (q_desc quiz)
    ++ table quiz answers) "haqu"
  where
    table quiz answers = e "TABLE" (mconcat $ tableHeader quiz : tableRows quiz answers ++ [tableSummary quiz answers])
    tableSummary quiz answers = e "TR" (mconcat $ e "TD" "Statistics" : map (tdCells quiz answers) [0..length (q_questions quiz) - 1])
  
tdCells :: Quiz -> QuizAnswers -> Int -> Html
tdCells q a i = e "TD" (show correctAnswersCount ++ "/" ++ show totalAnswersCount)
  where
    question = getQuestion q i
    correctAnswersCount = countCorrectAnswers question (flatAnswers (show i) (qa_player_answer a))
    totalAnswersCount = countTotalAnswersPerQuestion (flatAnswers (show i) (qa_player_answer a))

tDefTh :: Html
tDefTh = e "TH" "Player"

tableHeader :: Quiz -> Html
tableHeader q = e "TR" (mconcat $ tDefTh : map thCells [1..length (q_questions q)])

thCells :: Int -> Html
thCells i = e "TH" ("Q" ++ show i)

tableRows :: Quiz -> QuizAnswers -> [Html]
tableRows quiz answers = map (tableRow quiz) (qa_player_answer answers)

tableRow :: Quiz -> PlayerAnswers -> Html
tableRow quiz answer = e "TR" $ mconcat $
  e "TD" (pa_player answer) : map (answerCell quiz) (pa_answers answer)

answerCell :: Quiz -> QuizAnswer -> Html
answerCell quiz answer = ea "TD" [("class", getStyleClass quiz answer)] (qa_answer answer)

getStyleClass :: Quiz -> QuizAnswer -> String
getStyleClass quiz answer = if isAnswerCorrect question qaAnswer
  then "correct"
  else "wrong"
  where
    question = getQuestion quiz (read $ qa_questionId answer)
    qaAnswer = qa_answer answer

getQuestion :: Quiz -> Int -> Question
getQuestion quiz position = q_questions quiz !! position

countCorrectAnswers :: Question -> [QuizAnswer] -> Int
countCorrectAnswers question answers = length $ filter (isAnswerCorrect question) (map qa_answer answers)

countTotalAnswersPerQuestion :: [QuizAnswer] -> Int
countTotalAnswersPerQuestion a = length (map qa_answer a)

delNonMatchAns :: String -> [QuizAnswer] -> [QuizAnswer]
delNonMatchAns q = filter (\a -> qa_questionId a == q)

flatAnswers :: String -> [PlayerAnswers] -> [QuizAnswer]
flatAnswers q a = delNonMatchAns q (concatMap pa_answers a)

isAnswerCorrect :: Question -> String -> Bool
isAnswerCorrect question answer = q_solution question == answer


quizAction :: ActionM ()
quizAction = do htmlString $ e "H1" "Bitte wähle ein Quiz aus:"
