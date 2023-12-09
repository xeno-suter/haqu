{-# LANGUAGE OverloadedStrings #-}

module Haqu.Pages.QuizResult (resultAction) where

import Control.Monad.IO.Class (liftIO)
import Haqu.Model.Quiz
import Haqu.Model.Answer
import Haqu.DataHandler
import Haqu.Components.Helper
import Web.Scotty

type Html = String

-- Quiz Result Action
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
  
-- Hilfsfunktionen zum Rendern der Tabelle
tdCells :: Quiz -> QuizAnswers -> Int -> Html
tdCells q a i = e "TD" (show correctAnswersCount ++ "/" ++ show totalAnswersCount)
  where
    question = getQuestion q i
    correctAnswersCount = countCorrectAnswers question (flatAnswers (show i) (qa_player_answer a))
    totalAnswersCount = countTotalAnswersPerQuestion (flatAnswers (show i) (qa_player_answer a))

-- Hilfsfunktionen zum Rendern der Tabelle (Header)
tDefTh :: Html
tDefTh = e "TH" "Player"

-- Hilfsfunktionen zum Rendern der Tabelle (Header)
tableHeader :: Quiz -> Html
tableHeader q = e "TR" (mconcat $ tDefTh : map thCells [1..length (q_questions q)])

-- Hilfsfunktionen zum Rendern der Tabelle (Header - Zelle)
thCells :: Int -> Html
thCells i = e "TH" ("Q" ++ show i)

-- Hilfsfunktionen zum Rendern der Tabelle (Zeilen)
tableRows :: Quiz -> QuizAnswers -> [Html]
tableRows quiz answers = map (tableRow quiz) (qa_player_answer answers)

-- Hilfsfunktionen zum Rendern der Tabelle (Zeile)
tableRow :: Quiz -> PlayerAnswers -> Html
tableRow quiz answer = e "TR" $ mconcat $
  e "TD" (pa_player answer) : map (answerCell quiz) (pa_answers answer)

-- Hilfsfunktionen zum Rendern der Tabelle (Zelle)
answerCell :: Quiz -> QuizAnswer -> Html
answerCell quiz answer = ea "TD" [("class", getStyleClass quiz answer)] (qa_answer answer)

-- Hilfsfunktionen zum Rendern der Tabelle (Zelle - Style)
getStyleClass :: Quiz -> QuizAnswer -> String
getStyleClass quiz answer = if isAnswerCorrect question qaAnswer
  then "correct"
  else "wrong"
  where
    question = getQuestion quiz (read $ qa_questionId answer)
    qaAnswer = qa_answer answer

-- Hilfsfunktionen zum finden der Aktuellen Frage anhand der Position
getQuestion :: Quiz -> Int -> Question
getQuestion quiz position = q_questions quiz !! position

-- Hilfsfunktion zum Zählen der richtigen Antworten
countCorrectAnswers :: Question -> [QuizAnswer] -> Int
countCorrectAnswers question answers = length $ filter (isAnswerCorrect question) (map qa_answer answers)

-- Hilfsfunktion zum Zählen der Antworten pro Frage
countTotalAnswersPerQuestion :: [QuizAnswer] -> Int
countTotalAnswersPerQuestion a = length (map qa_answer a)

-- Hilfsfunktion zum Löschen von nicht passenden Antworten (nach Frage)
delNonMatchAns :: String -> [QuizAnswer] -> [QuizAnswer]
delNonMatchAns q = filter (\a -> qa_questionId a == q)

-- Hilfsfunktion zum "flachen" machen der Antworten
flatAnswers :: String -> [PlayerAnswers] -> [QuizAnswer]
flatAnswers q a = delNonMatchAns q (concatMap pa_answers a)

-- Hilfsfunktion zum Prüfen ob eine Antwort richtig ist
isAnswerCorrect :: Question -> String -> Bool
isAnswerCorrect question answer = q_solution question == answer