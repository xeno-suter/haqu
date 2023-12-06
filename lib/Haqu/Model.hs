module Haqu.Model (
    Question,
    Quiz,
    QuestionType,
    QuizAnswer,
    PlayerAnswers,
    QuizAnswers
) where

-- Datenstruktur für Frage Typen
data QuestionType = SINGLECHOICE | FALSETRUE deriving Show

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

-- Datenstruktur für eine Quizantwort
data QuizAnswer = QuizAnswer {
    qa_questionId :: String,
    qa_answers :: [String]
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