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
    table quiz answers = e "TABLE" (mconcat ((tableHeader quiz) : (tableRows answers)))
    tableHeader quiz = e "TR" (mconcat $ e "TH" "Player" : map (\q -> e "TH" ("Q" ++ show q)) [1..length (q_questions quiz)])
    tableRows answers = map tableRow (qa_player_answer answers)
    tableRow answer = e "TR" (mconcat $ e "TD" (pa_player answer) : map (\a -> ea "TD" [("class", "correct")] (qa_answer a)) (pa_answers answer))

isAnswerCorrect :: Question -> QuizAnswer -> Bool
isAnswerCorrect question answer = q_solution question == qa_answer answer

quizAction :: ActionM ()
quizAction = do htmlString $ e "H1" "Bitte wähle ein Quiz aus:"
