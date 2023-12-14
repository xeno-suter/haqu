{-# LANGUAGE OverloadedStrings #-}

-- Module: Haqu.Pages.QuizQuestion - Aktion der Frage Seiten
module Haqu.Pages.QuizQuestion (questionAction) where

import qualified Data.Text.Lazy as LT
import Web.Scotty
    ( ActionM, captureParam, formParam, redirect, queryParam )
import Haqu.Components.Helper
import Control.Monad.IO.Class (liftIO)
import Haqu.Model.Quiz
import Haqu.DataHandler
import Haqu.Components.Form

-- Typdefinition für HTML
type Html = String

-- Quiz Question Method Dispatcher
questionAction :: String -> ActionM ()
questionAction "get" = handleGetRequest
questionAction "post" = handlePostRequest
questionAction _ = error "Unknown method"

-- Quiz Question Form
handleGetRequest :: ActionM ()
handleGetRequest = do
  quizId <- captureParam "quiz"
  questionId <- captureParam "question"
  quiz <- liftIO $ readQuizFile ("data/" ++ quizId ++ ".txt")
  htmlString $ htmlDoc (formWrapper "Submit answer" (questionInput quiz questionId)) "haqu"

-- Quiz Question Redirect zur nächsten Frage oder zum Ergebnis
handlePostRequest :: ActionM ()
handlePostRequest = do
  quizId <- captureParam "quiz"
  qId <- captureParam "question"
  p <- queryParam "player"
  answer <- formParam "answer"
  quiz <- liftIO $ readQuizFile ("data/" ++ quizId ++ ".txt")
  done <- liftIO $ storeAnswer quizId p qId answer
  if done then
    if hasNextQuestion quiz qId then
        redirect $ LT.pack $ "/quiz/" ++ quizId ++ "/" ++ show (qId + 1) ++ "?player=" ++ p
    else
      redirect $ LT.pack $ "/quiz/" ++ quizId ++ "/result"
  else
    error "Could not store answer"

-- Hilfsfunktion zum Prüfen ob es noch eine nächste Frage gibt
hasNextQuestion :: Quiz -> Int -> Bool
hasNextQuestion quiz position = length (q_questions quiz) > position + 1

-- Hilfsfunktion zum Rendern der Frage anhand des Typs
questionInput :: Quiz -> Int -> Html
questionInput quiz qId
  | q_type question == FALSETRUE = trueFalseQuestion question
  | q_type question == SINGLECHOICE = singleChoiceQuestion question
  | otherwise = error "Unknown question type"
  where
    question = getQuestion quiz qId

-- Hilffunktion zum finden der Aktuellen Frage anhand der Position
getQuestion :: Quiz -> Int -> Question
getQuestion quiz position = q_questions quiz !! position