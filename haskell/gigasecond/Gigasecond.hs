module Gigasecond (fromDay) where
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX

fromDay :: UTCTime -> UTCTime
fromDay date = posixSecondsToUTCTime (((utcTimeToPOSIXSeconds (date))) + 1000000000)