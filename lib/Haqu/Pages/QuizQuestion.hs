{-# LANGUAGE OverloadedStrings #-}

module Haqu.Pages.QuizQuestion (questionAction) where

import qualified Data.Text.Lazy as LT
import Web.Scotty
    ( ActionM, captureParam, formParam, redirect, queryParam )
import Haqu.Components.Helper
import Control.Monad.IO.Class (liftIO)
import Haqu.Model.Quiz
import Haqu.FileReader
import Haqu.Components.Form

type Html = String

questionAction :: String -> ActionM ()
questionAction "get" = handleGetRequest
questionAction "post" = handlePostRequest
questionAction _ = error "Unknown method"

handleGetRequest :: ActionM ()
handleGetRequest = do
  quizId <- captureParam "quiz"
  questionId <- captureParam "question"
  quiz <- liftIO $ readQuizFile ("data/" ++ quizId ++ ".txt")
  htmlString $ htmlDoc (formWrapper "Submit" (questionInput quiz questionId)) "haqu"

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