{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

import Web.Scotty

import Data.Text
import Data.Time

import GHC.Generics

import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Cors
import Data.Maybe (fromMaybe)
import Data.Aeson (encode)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import GraphQL
import GraphQL.API
import GraphQL.Resolver
import GraphQL.Value.ToValue (ToValue(..))

data Countdown = Countdown { short :: Text, description :: Text, startDate :: UTCTime, endDate :: UTCTime } deriving (Show,Generic)

type CountdownQuery = Object "Countdown" '[]
  '[Field "short" Text
  , Field "description" Text
  , Field "startDate" Text
  , Field "endDate" Text
  ]

type QueryRoot = Object "QueryRoot" '[]
  '[Field "countdown" CountdownQuery]
  -- '[ Argument "short" Text :> Field "countdown" Countdown
  -- , Argument "description" Text :> Argument "short" (Maybe Text) :> Argument "startDate" String :> Argument "endDate" String :> Field "createCountdown" Countdown ]

testCountdown = Countdown
  { short       = "blah"
  , description = "Diddy doo dah"
  , startDate   = UTCTime (fromGregorian 2018 3 16) 0
  , endDate     = UTCTime (fromGregorian 2018 3 23) 0
  }

lookupCountdown :: Text -> IO (Maybe Countdown)
lookupCountdown _ = pure $ Just Countdown
  { short       = "blah"
  , description = "Diddy doo dah"
  , startDate   = UTCTime (fromGregorian 2018 3 16) 0
  , endDate     = UTCTime (fromGregorian 2018 3 23) 0
  }
-- lookupCountdown :: Text -> IO (Maybe Countdown)
-- lookupCountdown _ = do
--   startDate <- getCurrentTime
--   return $ Just Countdown
--     { short       = "blah"
--     , description = "Diddy doo dah"
--     , startDate   = startDate
--     , endDate     = getEndDate startDate
--     }
--   where getEndDate (UTCTime day time) = UTCTime (addDays 7 day) time

countdownHandler :: Text -> Handler IO (Maybe CountdownQuery)
countdownHandler short = do
  countdown <- lookupCountdown short
  traverse (pure . viewCountdown) countdown

viewCountdown :: Countdown -> Handler IO CountdownQuery
viewCountdown countdown@Countdown {..} =
  pure
    $   pure short
    :<> pure description
    :<> pure (pack $ toStringTime startDate)
    :<> pure (pack $ toStringTime endDate)
 where
  toStringTime =
    formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S")

createCountdownHandler
  :: Text -> Maybe Text -> String -> String -> Handler IO CountdownQuery
createCountdownHandler description short startDate endDate = viewCountdown
  Countdown
    { short       = fromMaybe "blah" short
    , description = description
    , startDate   = (read startDate) :: UTCTime
    , endDate     = (read endDate) :: UTCTime
    }


handler :: Handler IO QueryRoot
handler = pure (viewCountdown testCountdown)
-- handler = pure $ countdownHandler :<> createCountdownHandler

runQuery :: Text -> IO Response
runQuery = interpretAnonymousQuery @QueryRoot handler

main :: IO ()
main = scotty 3000 $ do
  middleware simpleCors
  post "/graphql" $ do
    b        <- body
    response <- liftAndCatchIO $ runQuery $ toQuery b
    json $ toValue response
  where toQuery b = toStrict $ decodeUtf8 b
