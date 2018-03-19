{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

import System.Random
import Data.Aeson hiding (Object)
import Web.Scotty
import Data.Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time
import GHC.Generics
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Cors
import Data.Maybe (fromMaybe)
import Data.Aeson (encode)
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy.Encoding as Lazy
import GraphQL
import GraphQL.API
import GraphQL.Resolver
import GraphQL.Value.ToValue (ToValue(..))
import qualified Database.PostgreSQL.Simple as DB
import Database.PostgreSQL.Simple hiding (query, Query)
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Web.Hashids

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
    DB.query
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
                                    (iso8601DateFormat $ Just "%H:%M:%SZ")
                                    (unpack str)

extractCountdown :: [Countdown] -> Countdown
extractCountdown [countdown] = countdown

makeShort :: Text -> IO Text
makeShort salt = do
  randomNums <- sequence $ replicateM 5 randomRIO (1, 9 :: Int)
  return $ decodeUtf8 $ encodeListUsingSalt (encodeUtf8 salt) randomNums


createCountdownHandler
  :: Connection
  -> Text
  -> Maybe Text
  -> Text
  -> Text
  -> Handler IO CountdownQuery
createCountdownHandler conn description maybeShort startDate endDate = do
  actualShort <- getOrMakeShort maybeShort
  rows        <- DB.query conn q $ buildCountdown actualShort
  viewCountdown $ extractCountdown (rows :: [Countdown])
 where
  buildCountdown short = Countdown
    { short       = short
    , description = description
    , startDate   = parseUtcTime startDate
    , endDate     = parseUtcTime endDate
    }
  getOrMakeShort (Just short) = pure short
  getOrMakeShort Nothing      = makeShort description
  q
    = "insert into timers (short, description, starts_on, ends_on) values (?,?,?,?) returning short, description, starts_on, ends_on"

handler :: Connection -> Handler IO QueryRoot
-- handler = pure (viewCountdown testCountdown)
-- handler = pure countdownHandler
handler conn = pure $ (countdownHandler conn) :<> (createCountdownHandler conn)

runQuery :: Connection -> Text -> IO Response
runQuery conn = interpretAnonymousQuery @QueryRoot $ handler conn

corsPolicy :: Maybe CorsResourcePolicy
corsPolicy = Just CorsResourcePolicy
  { corsOrigins        = Nothing
  , corsMethods        = simpleMethods
  , corsRequestHeaders = ["content-type"]
  , corsExposedHeaders = Nothing
  , corsMaxAge         = Nothing
  , corsVaryOrigin     = False
  , corsRequireOrigin  = False
  , corsIgnoreFailures = False
  }

data QueryDoc = Query { query :: Text } deriving (Show, Generic)
instance FromJSON QueryDoc

main :: IO ()
main = scotty 4000 $ do
  middleware $ cors $ const corsPolicy
  post "/graphql" $ do
    conn <- liftIO $ connectPostgreSQL "postgres://localhost/countdown-to_dev"
    b <- jsonData
    response <- liftAndCatchIO $ runQuery conn $ query (b :: QueryDoc)
    Web.Scotty.json $ toValue response
