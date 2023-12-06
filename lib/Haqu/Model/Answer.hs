module Haqu.Model.Answer (
    QuizAnswer(..),
    PlayerAnswers(..),
    QuizAnswers(..)
) where

-- Datenstruktur für eine Quizantwort
data QuizAnswer = QuizAnswer {
    qa_questionId :: String,
    qa_answers :: String
} deriving Show

-- Datenstruktur für Quizantworten eines Spielers
data PlayerAnswers = PlayerAnswers {
    pa_player :: String,
    pa_answers :: [QuizAnswer]
} deriving Show

-- Datenstruktur für Quizantworten aller Spieler
data QuizAnswers = QuizAnswers {
    qa_quizId :: String,
    qa_player_answer :: [PlayerAnswers]
} deriving Show