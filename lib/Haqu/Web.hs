{-# LANGUAGE OverloadedStrings #-}

module Haqu.Web where
import Web.Scotty
    ( file, get, middleware, scotty, setHeader, ActionM, captureParam, post, formParam, queryParam, redirect )
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class (liftIO)
import Haqu.Model.Quiz
import Haqu.Model.Answer
import Haqu.FileReader
import Haqu.Components.Helper
import Haqu.Components.Form
import Haqu.Pages.Home
import Haqu.Pages.QuizPlayer
import qualified Data.Text.Lazy as LT

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


questionAction :: String -> ActionM ()
questionAction "get" = do
  quizId <- captureParam "quiz"
  questionId <- captureParam "question"
  quiz <- liftIO $ readQuizFile ("data/" ++ quizId ++ ".txt")
  htmlString $ htmlDoc (formWrapper "Submit" (questionInput quiz questionId)) "haqu"
questionAction "post" = do
  quizId <- captureParam "quiz"
  questionId <- captureParam "question"
  player <- queryParam "player"
  answer <- formParam "answer"
  quiz <- liftIO $ readQuizFile ("data/" ++ quizId ++ ".txt")
  done <- liftIO $ storeAnswer quizId player questionId answer
  if done then
    if hasNextQuestion quiz questionId then
        redirect $ LT.pack $ "/quiz/" ++ quizId ++ "/" ++ show (questionId + 1) ++ "?player=" ++ player
    else
      redirect $ LT.pack $ "/quiz/" ++ quizId ++ "/result"
  else
    error "Could not store answer"
questionAction _ = error "Unknown method"

hasNextQuestion :: Quiz -> Int -> Bool
hasNextQuestion quiz position = length (q_questions quiz) > position + 1


questionInput :: Quiz -> Int -> Html
questionInput quiz qId
  | q_type question == FALSETRUE = trueFalseQuestion question
  | q_type question == SINGLECHOICE = singleChoiceQuestion question
  | otherwise = error "Unknown question type"
  where
    question = getQuestion quiz qId

getQuestion :: Quiz -> Int -> Question
getQuestion quiz position = q_questions quiz !! position

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
    table quiz answers = e "TABLE" (mconcat ((tableHeader quiz) : (tableRows answers)))
    tableHeader quiz = e "TR" (mconcat $ e "TH" "Player" : map (\q -> e "TH" ("Q" ++ show q)) [1..length (q_questions quiz)])
    tableRows answers = map tableRow (qa_player_answer answers)
    tableRow answer = e "TR" (mconcat $ e "TD" (pa_player answer) : map (\a -> ea "TD" [("class", "correct")] (qa_answer a)) (pa_answers answer))

isAnswerCorrect :: Question -> QuizAnswer -> Bool
isAnswerCorrect question answer = q_solution question == qa_answer answer

quizAction :: ActionM ()
quizAction = do htmlString $ e "H1" "Bitte wähle ein Quiz aus:"
