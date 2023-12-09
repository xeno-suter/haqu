{-# LANGUAGE OverloadedStrings #-}

module Haqu.Web where
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Haqu.Components.Helper
import Haqu.Pages.Home
import Haqu.Pages.QuizPlayer
import Haqu.Pages.QuizQuestion
import Web.Scotty
import Haqu.Pages.QuizResult

type Html = String

-- Main Funktion mit Routing
main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev

  get  "/styles.css" styles
  get  "/" homeAction

  get "/quiz" quizAction

  get "/quiz/:id/start" (quizNameAction "get")
  post "/quiz/:id/start" (quizNameAction "post")

  get "/quiz/:id/result" resultAction

  get "/quiz/:quiz/:question" (questionAction "get")
  post "/quiz/:quiz/:question" (questionAction "post")

-- Stylesheet Action
styles :: ActionM ()
styles = do
    setHeader "Content-Type" "text/css"
    file "static/styles.css"

-- Quiz Action (Quiz wenn keine ID angegeben...)
quizAction :: ActionM ()
quizAction = do htmlString $ e "H1" "Bitte wähle ein Quiz aus:"
