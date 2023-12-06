module Haqu.QuizFileReader (readQuizFile) where

import Data.List
import Data.Char (isSpace)
import Haqu.Model
import Prelude hiding (id)

-- Funktion zum Einlesen einer Quizdatei
readQuizFile :: FilePath -> IO Quiz
readQuizFile filePath = do
    let id = extractQuizId filePath
    content <- readFile filePath
    return $ parseQuiz id content

-- Funktion zum extrahieren der Quiz ID
extractQuizId :: FilePath -> String
extractQuizId = takeWhile (/= '.') . reverse .(takeWhile (/= '/') . reverse)

-- Funktion zum Parsen einer Quizdatei
parseQuiz :: String -> String -> Quiz
parseQuiz id content = Quiz id name desc questions
  where
    (name, desc, questions) = parseQuizContent (lines content) ("", "", [])

-- Funktion zum Parsen des Inhalts einer Quizdatei
parseQuizContent :: [String] -> (String, String, [Question]) -> (String, String, [Question])
parseQuizContent [] acc = acc
parseQuizContent (l:ls) (n, d, q)
    | "NAME:" `isPrefixOf` l = parseQuizContent ls (getValue l "NAME:", d, q)
    | "DESC:" `isPrefixOf` l = parseQuizContent ls (n, getValue l "DESC:", q)
    | "TYPE:" `isPrefixOf` l = parseQuizContent remainingLines (n, d, q ++ [parseQuestion questionLines])
    | otherwise = parseQuizContent ls (n, d, q)
  where
    (questionLines, remainingLines) = break null (l : ls)

-- Funktion zum Parsen einer Frage
parseQuestion :: [String] -> Question
parseQuestion lines = Question qType qText ans sOpt
  where
    qType = case getValue (head lines) "TYPE:" of
        "SINGLECHOICE" -> SINGLECHOICE
        "FALSETRUE"    -> FALSETRUE
        other          -> error $ "Unknown Question Type: " ++ other
    qText = getValue (head $ dropWhile (not . ("Q:" `isPrefixOf`)) lines) "Q:"
    ans = map (`getValue` "A:") (filter ("A:" `isPrefixOf`) lines)
    sOpt = getValue (head lines) "S:"

-- Hilfsfunktion zum Extrahieren des Werts aus einem "Key-Value-Paar"
getValue :: String -> String -> String
getValue line key = dropWhile isSpace (drop (length key) line)