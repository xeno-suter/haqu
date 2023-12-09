module Haqu.FileReader (
  readQuizFile,
  readPlayerAnswers,
  readQuizAnswers,
  storeAnswer
) where

import Data.List
import Data.Char (isSpace)
import Haqu.Model.Answer
import Haqu.Model.Quiz
import Haqu.Model.KeyValue
import System.Directory ( doesDirectoryExist, createDirectory, listDirectory )
import Control.Monad

-- Common Funktion für File lesen und parsen
readFileAndParse :: (String -> String -> a) -> FilePath -> IO a
readFileAndParse parse filePath = do
    let fileName = extractFileName filePath
    content <- readFile filePath
    return $ parse fileName content

-- Funktion zum Einlesen einer Antwortdatei
readPlayerAnswers :: FilePath -> IO PlayerAnswers
readPlayerAnswers = readFileAndParse parseAnswers

readQuizAnswers :: String -> IO QuizAnswers
readQuizAnswers quizId = do
  let path = "data/" ++ quizId ++ "/"
  playerAnswers <- readAllFilesWithExtension ".txt" path
  answers <- mapM readPlayerAnswers playerAnswers
  return $ QuizAnswers quizId answers

-- Funktion zum Einlesen einer Quizdatei
readQuizFile :: FilePath -> IO Quiz
readQuizFile = readFileAndParse parseQuiz

readAllFilesWithExtension :: String -> FilePath -> IO [FilePath]
readAllFilesWithExtension ext path = do
  files <- listDirectory path
  return $ map (path ++) (filter (ext `isSuffixOf`) files)

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
parseQuiz quizId content = Quiz quizId name desc questions
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
    questionLines = l:ls
    remainingLines = ls

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
    sOpt = concatMap (`getValue` "S:") (filter ("S:" `isPrefixOf`) lines)

-- Hilfsfunktion zum Extrahieren des Werts aus einem "Key-Value-Paar"
getValue :: String -> String -> String
getValue line key = dropWhile isSpace (drop (length key) line)

-- Hilfsfunktion zum Aufteilen eines Strings anhand eines Trennzeichens
parseKeyValue :: String -> KeyValue
parseKeyValue str = case break (== ':') str of
    (k, ':' : v) -> KeyValue k v
    _            -> error "Invalid key-value format"


----------------------------------

storeAnswer :: String -> String -> Int -> String -> IO Bool
storeAnswer quizId pName qNumber qAnswer = do
  let path = "data/" ++ quizId ++ "/" ++ pName ++ ".txt"
  appendFile path (show qNumber ++ ":" ++ qAnswer ++ "\n")
  return True

checkIfDirExistsElseCreate :: FilePath -> IO ()
checkIfDirExistsElseCreate path = do
  dirExists <- doesDirectoryExist path
  unless dirExists $ createDirectory path