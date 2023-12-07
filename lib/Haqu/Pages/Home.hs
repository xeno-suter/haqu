module Haqu.Pages.Home (homeAction) where

import Haqu.Components.Helper
import Web.Scotty (ActionM)
import Control.Monad.IO.Class (liftIO)
import Haqu.Model.Quiz
import Haqu.FileReader

homeAction :: ActionM ()
homeAction = do
  quizzList <- liftIO quizzes
  htmlString $ htmlDoc (e "UL" (concatMap quizItem quizzList)) "haqu"
  where
    quizItem quiz = e "LI" (quizTitle quiz ++ " " ++ quizStartLink quiz)
    quizTitle quiz = e "B" (quizId quiz ++ " " ++ q_name quiz) ++ " " ++ quizDesc quiz
    quizStartLink quiz = ea "A" [("href", quizStartPath quiz)] "Start"
    quizId quiz = "[" ++ q_id quiz ++ "]"
    quizStartPath quiz = "/quiz/" ++ q_id quiz ++ "/start"
    quizDesc quiz = e "SPAN" (q_desc quiz)


quizPaths :: [FilePath]
quizPaths = ["data/q0.txt", "data/q1.txt", "data/q2.txt"]

quizzes :: IO [Quiz]
quizzes = mapM readQuizFile quizPaths
