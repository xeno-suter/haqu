module Haqu.Components.Helper (
  htmlString,
  htmlDoc,
  e,
  ea
) where
  
import qualified Data.Text.Lazy as LT
import Data.List
import Web.Scotty (ActionM, html)

type Html = String

-- Standard HTML Head
htmlDefaultHead :: Html
htmlDefaultHead = htmlHead (htmlStylesheet "/styles.css")

-- Html Body Wrapper mit Titel
htmlBody :: Html -> String -> Html
htmlBody content title = e "BODY" (e "H1" title ++ content)

-- Html Head
htmlHead :: Html -> Html
htmlHead = e "HEAD"

-- Html Stylesheet links
htmlStylesheet :: Html -> Html
htmlStylesheet href = ea "LINK" [("rel", "stylesheet"), ("href", href)] ""

-- Html Dokument Wrapper
htmlDoc :: Html -> String -> Html
htmlDoc body title = e "html" (htmlDefaultHead ++ htmlBody body title)

-- Html "String" Wrapper
htmlString :: String -> ActionM ()
htmlString = html . LT.pack

-- Html DSL
e :: String -> Html -> Html
e tag = ea tag []

-- Html DSL mit Attributen
ea :: String -> [(String, String)] -> Html -> Html
ea tag attrs kids = concat $ ["<", tag] ++ attrsHtml attrs ++ [">", kids, "</", tag, ">"]
  where attrsHtml [] = []
        attrsHtml as = " " : intersperse " " (map attrHtml as)
        attrHtml (key, value) = key ++ "='" ++ value ++ "'"