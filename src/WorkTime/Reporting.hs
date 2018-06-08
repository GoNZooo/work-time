-- | Module for reporting functions used to extract more direct information
-- from the 'WorkTime' datatype.
module WorkTime.Reporting
    ( hoursFromText
    , hoursFromFile
    , hours
    )
where

import           Control.Applicative ((<$>))
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Text           (Text)
import qualified Data.Text.IO        as TIO
import           Data.Void           (Void)
import           Text.Megaparsec     (ParseError, Token)
import           WorkTime.WorkTime   (WorkTime, fromText, workTimeHours,
                                      workTimeNickname)

-- | Takes a text and turns it into a list of a nickname ('Text') and work hours
-- ('Double').
hoursFromText :: Text -> Either (ParseError (Token Text) Void) [(Text, Double)]
hoursFromText = (Map.toList . hours <$>) . fromText

-- | Takes a 'FilePath' and reads the 'Text' in the file, from which it then
-- parses a list of @[('Text', 'Double')]@ which represent a nickname and the
-- parsed total amount of hours they have worked.
hoursFromFile
    :: FilePath -> IO (Either (ParseError (Token Text) Void) [(Text, Double)])
hoursFromFile = fmap hoursFromText . TIO.readFile

-- | Takes a list of 'WorkTime' and creates a map of nickname
-- ('Text') to work hours ('Double').
hours :: [WorkTime] -> Map Text Double
hours = Map.fromListWith (+) . map entryHours

entryHours :: WorkTime -> (Text, Double)
entryHours workTime = (workTimeNickname workTime, workTimeHours workTime)
