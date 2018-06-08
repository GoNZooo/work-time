{-# LANGUAGE OverloadedStrings #-}

-- | The main data type. Represents all information in one message detailing
-- the work someone has done.
module WorkTime.WorkTime
  ( WorkTime(..)
  , MessageLine
  , Workday
  , fromText
  , fromFile
  , workTimeHours
  , workTimeNickname
  )
where

import           Data.Char                  (isDigit)
import           Data.Either
import qualified Data.Map                   as Map
import           Data.Text                  (Text, pack)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Void                  (Void)
import           Text.Megaparsec
import qualified Text.Megaparsec            as MP
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text

-- | Main data type of the library. Represents an entry in Slack containing
-- a nickname, timestamp, datestamp and 'WorkDay' that themselves contain
-- several task descriptions.
--
-- An entire entry will look as follows:
--
-- > Rickard Andersson [11:16 PM]
-- > [28.02] 8.5h
-- > worked on missile guidance system
-- > cleaned up parsing code
--
-- The first part is represented by a 'MessageLine' and what follows is one
-- 'Workday' in this example.
--
-- An entry can also look as follows:
--
-- > Rickard Andersson [11:16 PM]
-- > [28.02] 8.5h
-- > worked on missile guidance system
-- > cleaned up parsing code
-- > [01.03] 6h
-- > fixed critical bug in missile guidance system
-- > removed half of parsing code
--
-- In this example we have several 'Workday' in one 'WorkTime'.
data WorkTime = WorkTime MessageLine [Workday] deriving (Show)

-- | Extracts the total work hours from a 'WorkTime' entry, from all workdays
-- in the entry.
workTimeHours :: WorkTime -> Double
workTimeHours (WorkTime _ wds) = sum $ map workdayHours wds

-- | Extracts the nickname from a 'WorkTime' entry.
workTimeNickname :: WorkTime -> Text
workTimeNickname (WorkTime (MessageLine (Nickname n) _) _) = n

-- | Parses a 'Text' either into a ['WorkTime'] or into an error message in the
-- form of a 'String'. The error message may be less than informative as it comes
-- straight from the parser ("Text.Megaparsec").
fromText :: Text -> Either (ParseError (Token Text) Void) [WorkTime]
fromText = runParser workTimesP ""

-- | Parses the 'Text' in a file and returns either a ['WorkTime'] or an error
-- message in the form of a 'String'. The error message may be less than
-- informative as it come straight from the parser ("Text.Megaparsec").
fromFile :: FilePath -> IO (Either (ParseError (Token Text) Void) [WorkTime])
fromFile filename = TIO.readFile filename >>= pure . fromText

-- | Represents a line like @Rickard Andersson [10:48 AM]@ in Slack.
data MessageLine = MessageLine Nickname Timestamp deriving (Show)

-- | Represents a collection of lines like
--
-- > [28.02] 8.5h
-- > worked on missile guidance system
-- > cleaned up parsing code
--
-- in Slack.
data Workday = Workday WorkamountLine [TaskDescription] deriving (Show)

-- | Represents the sliver @[28.02] 8h@ in a whole 'MessageLine'.
data WorkamountLine = WorkamountLine Datestamp Workamount deriving (Show)
newtype TaskDescription = TaskDescription Text deriving (Show)
newtype Nickname = Nickname Text deriving (Show)

workdayHours :: Workday -> Double
workdayHours (Workday (WorkamountLine _ (Workamount amount)) _) = amount

data Timestamp = Timestamp Hours Minutes AMPM deriving (Show)
data AMPM = AM | PM deriving (Show)
newtype Hours = Hours Int deriving (Show)
newtype Minutes = Minutes Int deriving (Show)

data Datestamp = Datestamp Day Month deriving (Show)
newtype Day = Day Int deriving (Show)
newtype Month = Month Int deriving (Show)
newtype Workamount = Workamount Double deriving (Show)

workTimesP :: Parser [WorkTime]
workTimesP = workTimeP `sepBy1` newline

workTimeP :: Parser WorkTime
workTimeP = do
  messageLine <- messageLineP
  workdays    <- workdaysP
  pure $ WorkTime messageLine workdays

messageLineP :: Parser MessageLine
messageLineP = do
  nickname  <- nicknameP
  timestamp <- timestampP
  _         <- char ']'
  _         <- newline
  pure $ MessageLine nickname timestamp

workdaysP :: Parser [Workday]
workdaysP = some workdayP

workdayP :: Parser Workday
workdayP = do
  workamountLine   <- workamountLineP
  taskDescriptions <- taskDescriptionsP
  pure $ Workday workamountLine taskDescriptions

workamountLineP :: Parser WorkamountLine
workamountLineP = do
  _          <- char '['
  datestamp  <- datestampP
  _          <- char ']'
  _          <- spaceChar
  workamount <- workamountP
  pure $ WorkamountLine datestamp workamount

taskDescriptionsP :: Parser [TaskDescription]
taskDescriptionsP = some taskDescriptionP

taskDescriptionP :: Parser TaskDescription
taskDescriptionP = TaskDescription <$> takeDescription
 where
  takeDescription  = takeWhile1P Nothing (not . inClass "[\n") <* newline
  descriptionChars = syms ++ alphanum
  syms             = "- +_.,=&|(){}<>\"`'/:;$?!#^~%ł“=@"
  alphanum         = "a-zA-Z0-9"

workamountP :: Parser Workamount
workamountP = do
  amount <- eitherP (try float) (try decimal)
  _      <- char 'h'
  _      <- newline
  pure . Workamount $ either id fromInteger amount

datestampP :: Parser Datestamp
datestampP = do
  day   <- dayP
  _     <- char '.'
  month <- monthP
  pure $ Datestamp day month

dayP :: Parser Day
dayP = Day <$> decimal

monthP :: Parser Month
monthP = Month <$> decimal

whitespaceP :: Parser Char
whitespaceP = satisfy (inClass " \t")

hoursP :: Parser Hours
hoursP = Hours <$> decimal

minutesP :: Parser Minutes
minutesP = Minutes <$> decimal

timestampP :: Parser Timestamp
timestampP = do
  hours   <- hoursP
  _       <- char ':'
  minutes <- minutesP
  _       <- char ' '
  ampm    <- ampmP
  pure $ Timestamp hours minutes ampm

ampmP :: Parser AMPM
ampmP = do
  ampm <- eitherP (try $ string "AM") (try $ string "PM")
  case ampm of
    (Left  "AM") -> pure AM
    (Right "PM") -> pure PM

wordsP :: Parser Text
wordsP = mconcat <$> wordP `sepBy1` whitespaceP

wordP :: Parser Text
wordP = pack <$> some letterChar

nicknameP :: Parser Nickname
nicknameP = Nickname . pack <$> manyTill charLiteral (string " [")

inClass :: String -> Char -> Bool
inClass cs c = c `elem` cs
