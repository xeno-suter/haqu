-- Module: Haqu.Model.Answer Datenstruktur für Quizantworten
module Haqu.Model.Answer (
    QuizAnswer(..),
    PlayerAnswers(..),
    QuizAnswers(..)
) where

-- Datenstruktur für eine Quizantwort
data QuizAnswer = QuizAnswer {
    qa_questionId :: String,
    qa_answer :: String
} deriving Show

-- Datenstruktur für Quizantworten eines Spielers
data PlayerAnswers = PlayerAnswers {
    pa_player :: String,
    pa_answers :: [QuizAnswer]
} deriving Show

-- Datenstruktur für Quizantworten aller Spieler
data QuizAnswers = QuizAnswers {
    qa_quiz_id :: String,
    qa_player_answer :: [PlayerAnswers]
} deriving Show