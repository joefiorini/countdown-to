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
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow

data Countdown = Countdown { short :: Text, description :: Text, startDate :: UTCTime, endDate :: UTCTime } deriving (Show,Generic)

instance FromRow Countdown where
  fromRow = Countdown <$> field <*> field <*> field <*> field

instance ToRow Countdown where
  toRow c = [toField $ short c, toField $ description c, toField $ startDate c, toField $ endDate c]

type CountdownQuery = Object "Countdown" '[]
  '[Field "short" Text
  , Field "description" Text
  , Field "startDate" Text
  , Field "endDate" Text
  ]

type QueryRoot = Object "QueryRoot" '[]
  -- '[Field "countdown" CountdownQuery]
  -- '[ Argument "short" Text :> Field "countdown" (Maybe CountdownQuery) ]
  '[ Argument "short" Text :> Field "countdown" (Maybe CountdownQuery)
  , Argument "description" Text :> Argument "short" (Maybe Text) :> Argument "startDate" Text :> Argument "endDate" Text :> Field "createCountdown" CountdownQuery ]

testCountdown = Countdown
  { short       = "blah"
  , description = "Diddy doo dah"
  , startDate   = UTCTime (fromGregorian 2018 3 16) 0
  , endDate     = UTCTime (fromGregorian 2018 3 23) 0
  }

lookupCountdown :: Connection -> Text -> IO (Maybe Countdown)
lookupCountdown connection short = do
  rows <-
    query
        connection
        "select short, description, starts_on, ends_on from timers where short = ?"
      $ Only short
  return $ extractCountdown (rows :: [Countdown])
 where
  extractCountdown [countdown] = Just countdown
  extractCountdown _           = Nothing

countdownHandler :: Connection -> Text -> Handler IO (Maybe CountdownQuery)
countdownHandler conn short = do
  countdown <- lookupCountdown conn short
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

parseUtcTime :: Text -> UTCTime
parseUtcTime str = parseTimeOrError False
                                    defaultTimeLocale
                                    (iso8601DateFormat $ Just "%H:%M:%S")
                                    (unpack str)

extractCountdown :: [Countdown] -> Countdown
extractCountdown [countdown] = countdown

createCountdownHandler
  :: Connection
  -> Text
  -> Maybe Text
  -> Text
  -> Text
  -> Handler IO CountdownQuery
createCountdownHandler conn description short startDate endDate = do
  rows <- query conn q buildCountdown
  viewCountdown $ extractCountdown (rows :: [Countdown])
 where
  buildCountdown = Countdown
    { short       = getOrMakeShort short
    , description = description
    , startDate   = parseUtcTime startDate
    , endDate     = parseUtcTime endDate
    }
  getOrMakeShort (Just short) = short
  getOrMakeShort Nothing      = "blah"
  q
    = "insert into timers (short, description, starts_on, ends_on) values (?,?,?,?) returning short, description, starts_on, ends_on"

handler :: Connection -> Handler IO QueryRoot
-- handler = pure (viewCountdown testCountdown)
-- handler = pure countdownHandler
handler conn = pure $ (countdownHandler conn) :<> (createCountdownHandler conn)

runQuery :: Connection -> Text -> IO Response
runQuery conn = interpretAnonymousQuery @QueryRoot $ handler conn

main :: IO ()
main = scotty 3000 $ do
  middleware simpleCors
  post "/graphql" $ do
    conn <- liftIO $ connectPostgreSQL "postgres://localhost/countdown-to_dev"
    b <- body
    response <- liftAndCatchIO $ runQuery conn $ toQuery b
    json $ toValue response
  where toQuery b = toStrict $ decodeUtf8 b





















