{-# LANGUAGE OverloadedStrings #-}

module Haqu.Pages.QuizPlayer (quizNameAction) where

import qualified Data.Text.Lazy as LT
import Web.Scotty
    ( ActionM, captureParam, formParam, redirect )
import Haqu.Components.Helper
import Control.Monad.IO.Class (liftIO)
import Haqu.Model.Quiz
import Haqu.FileReader
import Haqu.Components.Form

-- Quiz Name Redirect
quizNameRedirect :: ActionM ()
quizNameRedirect = do
  quizId <- captureParam "id"
  player <- formParam "player"
  redirect $ LT.pack $ "/quiz/" ++ quizId ++ "/0?player=" ++ player

-- Quiz Name Form
quizNameForm :: ActionM ()
quizNameForm = do
  quizId <- captureParam "id"
  quiz <- liftIO $ readQuizFile ("data/" ++ quizId ++ ".txt")
  htmlString $ e "H1" "haqu" ++ dynComp quiz
  where
    quizTitle quiz = e "H2" ("Starting " ++ q_name quiz)
    playerForm = formWrapper "Start Quiz" playerName
    playerName = textInput "Please enter your name:" "player"
    dynComp quiz = e "DIV" (quizTitle quiz ++ playerForm)
    
-- Quiz Name Method Dispatcher
quizNameAction :: String -> ActionM ()
quizNameAction "post" = do quizNameRedirect
quizNameAction "get" = do quizNameForm
quizNameAction _ = error "Unknown method"
