-- | Main API for "WorkTime". This re-exports all functions meant to be used.
module WorkTime
    ( WorkTime(..)
    , MessageLine
    , Workday
    , fromText
    , workTimeHours
    , workTimeNickname
    , hoursFromText
    , hours
    )
where

import           WorkTime.Reporting
import           WorkTime.WorkTime
