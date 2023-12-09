-- Module: Haqu.Model.Quiz Datenstruktur für Quizze
module Haqu.Model.Quiz (
    Question(..),
    Quiz(..),
    QuestionType(..)
) where

-- Datenstruktur für Frage Typen
data QuestionType = SINGLECHOICE | FALSETRUE deriving (Show, Eq)

-- Datenstruktur für eine Frage
data Question = Question {
    q_type :: QuestionType,
    q_question :: String,
    q_answers :: [String],
    q_solution :: String
} deriving Show

-- Datenstruktur für eine Quiz
data Quiz = Quiz {
    q_id :: String,
    q_name :: String,
    q_desc :: String,
    q_questions :: [Question]
} deriving Show