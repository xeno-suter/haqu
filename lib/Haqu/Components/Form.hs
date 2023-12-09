module Haqu.Components.Form (
  singleChoiceQuestion,
  trueFalseQuestion,
  textInput,
  formWrapper
) where

import Haqu.Model.Quiz
import Haqu.Components.Helper

type Html = String

-- Funktion zum Erstellen der "Single Choice" Frage
singleChoiceQuestion :: Question -> Html
singleChoiceQuestion question = e "DIV" (label ++ options)
  where
    label = e "LABEL" $ q_question question
    options = e "DIV" $ mconcat (zipWith option [0..] (q_answers question))

-- Hilfsfunktion zum Erstellen der "Single Choice" Frage Option
option :: Int -> String -> Html
option index = inputField "radio" "answer" (show index)

-- Funktion zum Erstellen der "True/False" Frage
trueFalseQuestion :: Question -> Html
trueFalseQuestion question = inputGroup label (i1 ++ i2)
  where
    label = e "LABEL" (q_question question)
    i1 = inputField "radio" "answer" "True" "True"
    i2 = inputField "radio" "answer" "False" "False"

-- Formular Text Input Feld
textInput :: String -> String -> Html
textInput l n = inputGroup label input
  where
    label = e "LABEL" l
    input = inputField "text" n "" ""

-- Formular Input Feld
inputField :: String -> String -> String -> String -> Html
inputField iType n v l = e "DIV" field
  where
    field = ea "INPUT" [("type", iType), ("name", n), ("value", v)] l

-- Formular Gruppe mit Label und Input
inputGroup :: Html -> Html -> Html
inputGroup label input = ea "DIV" [] (labelWrapper ++ inputWrapper)
  where
    labelWrapper = ea "DIV" [("class", "label")] label
    inputWrapper = ea "DIV" [("class", "input")] input

-- Formular Wrapper mit Submit Button
formWrapper :: String -> Html -> Html
formWrapper submitLabel content = ea "FORM" [("METHOD", "POST")] kids
  where
    kids = content ++ submitButton
    submitButton = ea "BUTTON" [("type", "submit")] submitLabel
