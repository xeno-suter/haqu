module Haqu.Pages.Home (homeAction) where

import Haqu.Components.Helper
import Web.Scotty (ActionM)
import Control.Monad.IO.Class (liftIO)
import Haqu.Model.Quiz
import Haqu.DataHandler

type Html = String

-- Home Aktion mit liste aller Quizzes
homeAction :: ActionM ()
homeAction = do
  quizzes <- liftIO readAllQuizFiles
  htmlString $ htmlDoc (quizList quizzes) "haqu"

-- Liste aller Quizze
quizList :: [Quiz] -> Html
quizList quizzes = e "UL" (concatMap quizItem quizzes)
  where
    quizItem quiz = e "LI" (quizTitle quiz ++ " " ++ quizStartLink quiz)
    quizStartLink quiz = ea "A" [("href", quizStartPath quiz)] "Start"
    quizStartPath quiz = "/quiz/" ++ q_id quiz ++ "/start"

-- Hilfsfuntion zum Erstellen des Quiztitels
quizTitle :: Quiz -> Html
quizTitle q = e "B" (quizId q <> " " <> q_name q) <> " " <> quizDesc q
  where
    quizId quiz = e "SPAN" ("[" ++ q_id quiz ++ "]")
    quizDesc quiz = e "SPAN" (q_desc quiz)