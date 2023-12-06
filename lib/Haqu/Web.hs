{-# LANGUAGE OverloadedStrings #-}

module Haqu.Web where
import Web.Scotty
    ( file, get, html, middleware, scotty, setHeader, ActionM, captureParam, post, redirect, formParam )
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as LT
import Data.List
import Haqu.Model
import Haqu.QuizFileReader

type Html = String


main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev

  get  "/styles.css" styles
  get  "/" homeAction

  get "/quiz" quizAction

  get "/quiz/:id/start" (quizNameAction "get")
  post "/quiz/:id/start" (quizNameAction "post")


styles :: ActionM ()
styles = do
    setHeader "Content-Type" "text/css"
    file "static/styles.css"


quizNameRedirect :: ActionM ()
quizNameRedirect = do
  quizId <- captureParam "id"
  player <- formParam "player"
  redirect $ LT.pack $ "/quiz/" ++ quizId ++ "/question/0?player=" ++ player

quizNameForm :: ActionM ()
quizNameForm = do
  quizId <- captureParam "id"
  quiz <- liftIO $ readQuizFile ("data/" ++ quizId ++ ".txt")
  htmlString $ e "H1" "haqu" ++ dynComp quiz
  where
    quizTitle quiz = e "H2" ("Starting " ++ q_name quiz)
    nameForm = ea "FORM" [("METHOD", "POST"),  ("action", "#")] (label ++ input ++ submit)
    label = e "LABEL" "Please enter your name:"
    input = ea "INPUT" [("type", "text"), ("name", "player")] ""
    submit = ea "BUTTON" [("type", "submit")] "Start Quiz"
    dynComp quiz = e "DIV" (quizTitle quiz ++ nameForm)

quizNameAction :: String -> ActionM ()
quizNameAction "post" = do
  quizNameRedirect 
quizNameAction "get" = do quizNameForm
quizNameAction _ = error "Unknown method"

quizAction :: ActionM ()
quizAction = do
  htmlString $ e "H1" "Quiz"
  

homeAction :: ActionM ()
homeAction = do
    liftIO (putStrLn "DEBUG: Home Action Called")
    quizzList <- liftIO quizzes
    htmlString $ e "H1" "haqu" ++ e "UL" (concatMap quizItem quizzList)
    where
        quizItem quiz = e "LI" (quizDesc quiz ++ " " ++ quizStartLink quiz)
        quizDesc quiz = e "B" ("[" ++ q_id quiz ++ "] " ++ q_name quiz) ++ e "SPAN" (" " ++ q_desc quiz)
        quizStartLink quiz = ea "A" [("href", "/quiz/" ++ q_id quiz++ "/start")] "Start"
        
quizPaths :: [FilePath]
quizPaths = ["data/q0.txt", "data/q1.txt", "data/q2.txt"]

quizzes :: IO [Quiz]
quizzes = mapM readQuizFile quizPaths

htmlString :: String -> ActionM ()
htmlString = html . LT.pack


-- Html DSL
e :: String -> Html -> Html
e tag = ea tag []


ea :: String -> [(String, String)] -> Html -> Html
ea tag attrs kids = concat $ ["<", tag] ++ attrsHtml attrs ++ [">", kids, "</", tag, ">"]
  where attrsHtml [] = []
        attrsHtml as = " " : intersperse " " (map attrHtml as)
        attrHtml (key, value) = key ++ "='" ++ value ++ "'"
