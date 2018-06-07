-- | Module for reporting functions used to extract more direct information
-- from the 'WorkTime' datatype.
module WorkTime.Reporting
    ( hoursFromText
    , hours
    )
where

import           Control.Applicative            ( (<$>) )
import           WorkTime.WorkTime              ( fromText
                                                , workTimeHours
                                                , workTimeNickname
                                                , WorkTime
                                                )
import           Data.Text                      ( Text )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.IO             as TLIO

-- | Takes a text and turns it into a list of a nickname ('Text') and work hours
-- ('Double').
hoursFromText :: Text -> Either String [(Text, Double)]
hoursFromText = (Map.toList . hours <$>) . fromText

-- | Takes a list of 'WorkTime' and creates a map of nickname
-- ('Text') to work hours ('Double').
hours :: [WorkTime] -> Map Text Double
hours = Map.fromListWith (+) . map entryHours

entryHours :: WorkTime -> (Text, Double)
entryHours workTime = (workTimeNickname workTime, workTimeHours workTime)

testList :: IO ()
testList = do
    txt <- testText
    case fromText txt of
        (Right parsed) -> print . Map.toList $ hours parsed
        (Left  err   ) -> print err

testText :: IO Text
testText = TL.toStrict <$> TLIO.readFile "testdata.txt"
