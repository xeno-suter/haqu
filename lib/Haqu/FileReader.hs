module Haqu.FileReader (readQuizFile, readAnswerFile) where

import Data.List
import Data.Char (isSpace)
import Haqu.Model.Answer
import Haqu.Model.Quiz
import Haqu.Model.KeyValue

-- Common Funktion für File lesen und parsen
readFileAndParse :: (String -> String -> a) -> FilePath -> IO a
readFileAndParse parse filePath = do
    let fileName = extractFileName filePath
    content <- readFile filePath
    return $ parse fileName content

-- Funktion zum Einlesen einer Antwortdatei
readAnswerFile :: FilePath -> IO PlayerAnswers
readAnswerFile = readFileAndParse parseAnswers

-- Funktion zum Einlesen einer Quizdatei
readQuizFile :: FilePath -> IO Quiz
readQuizFile = readFileAndParse parseQuiz















-- Funktion zum Parsen der Antworten
parseAnswers :: String -> String -> PlayerAnswers
parseAnswers personName content = PlayerAnswers personName questions
  where
    questions = map parseAnswerLine (lines content)

-- Funktion zum Parsen einer Zeile im Antwortdateiformat
parseAnswerLine :: String -> QuizAnswer
parseAnswerLine line = QuizAnswer (kv_key keyValue) (kv_value keyValue)
  where
    keyValue = parseKeyValue line

-- Funktion zum extrahieren des Filenamens aus einem Pfad
extractFileName :: FilePath -> String
extractFileName = takeWhile (/= '.') . reverse .(takeWhile (/= '/') . reverse)

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

-- Hilfsfunktion zum Aufteilen eines Strings anhand eines Trennzeichens
parseKeyValue :: String -> KeyValue
parseKeyValue str = case break (== ':') str of
    (k, ':' : v) -> KeyValue k v
    _            -> error "Invalid key-value format"
