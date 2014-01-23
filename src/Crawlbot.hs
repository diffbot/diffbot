{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Crawlbot where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Char8 as BC
import           Data.List
import           Data.Maybe
import           Data.Time.Clock
import           Data.Time.Clock.POSIX

import           Data.Aeson
import           Network.HTTP.Types.QueryLike
import           Network.HTTP.Types.URI

import Types

data Crawlbot = Crawlbot
    { crawlbotName               :: String
    -- ^ Job name. This should be a unique identifier and can be used
    -- to modify your crawl or retrieve its output.
    , crawlbotSeeds              :: Maybe [String]
    -- ^ Seed URL(s). By default Crawlbot will spider subdomains
    -- (e.g., a seed URL of <http://www.diffbot.com> will include URLs
    -- at <http://blog.diffbot.com>).
    , crawlbotApi                :: Maybe Req
    -- ^ Diffbot API through which to process pages. E.g., 'Article'
    -- to process matching links via the Article API.

    , crawlbotUrlCrawlLimit      :: Maybe Limit
    -- ^ Limit crawled pages.
    , crawlbotUrlProcessLimit    :: Maybe Limit
    -- ^ Limit processed pages.
    , crawlbotPageProcessPattern :: Maybe [String]
    -- ^ Specify strings to limit pages processed to those whose HTML
    -- contains any of the content strings.

    , crawlbotMaxToCrawl         :: Maybe Int
    -- ^ Specify max pages to spider.
    , crawlbotMaxToProcess       :: Maybe Int
    -- ^ Specify max pages to process through Diffbot APIs. Default:
    -- 10,000.
    , crawlbotRestrictDomain     :: Bool
    -- ^ By default crawls will restrict to subdomains within the seed
    -- URL domain. Specify to 'False' to follow all links regardless
    -- of domain.
    , crawlbotNotifyEmail        :: Maybe String
    -- ^ Send a message to this email address when the crawl hits the
    -- 'crawlbotMaxToCrawl' or 'crawlbotMaxToProcess' limit, or when
    -- the crawl completes.
    , crawlbotNotifyWebHook      :: Maybe String
    -- ^ Pass a URL to be notified when the crawl hits the
    -- 'crawlbotMaxToCrawl' or 'crawlbotMaxToProcess' limit, or when
    -- the crawl completes. You will receive a POST with X-Crawl-Name
    -- and X-Crawl-Status in the headers, and the full JSON response
    -- in the POST body.
    , crawlbotDelay              :: Maybe Double
    -- ^ Wait this many seconds between each URL crawled from a single
    -- IP address.
    , crawlbotRepeat             :: Maybe Double
    -- ^ Specify the number of days as a floating-point
    -- (e.g. crawlbotRepeat=7.0) to repeat this crawl. By default
    -- crawls will not be repeated.
    , crawlbotOnlyProcessIfNew   :: Bool
    -- ^ By default repeat crawls will only process new (previously
    -- unprocessed) pages. Set to 'False' to process all content on
    -- repeat crawls.
    , crawlbotMaxRounds          :: Int
    -- ^ Specify the maximum number of crawl repeats. Use '-1' to
    -- continually repeat.
    }


instance Request Crawlbot where
    toReq a = Req { reqApi     = "http://api.diffbot.com/v2/crawl"
                  , reqContent = Nothing
                  , reqQuery   = mkCrawlbotQuery a
                  }


mkCrawlbotQuery :: Crawlbot -> [(String, Maybe String)]
mkCrawlbotQuery (Crawlbot {..}) =
    catMaybes [ mkQuery      "name"               (Just crawlbotName)
              , mkQuery      "seeds"              (unwords <$> crawlbotSeeds)
              , mkQueryApi   "apiUrl"             crawlbotApi
              , mkQueryLimit "urlCrawl"           crawlbotUrlCrawlLimit
              , mkQueryLimit "urlProcess"         crawlbotUrlProcessLimit
              , mkQuery      "pageProcessPattern" (unpatternStrings <$> crawlbotPageProcessPattern)
              , mkQuery      "maxToCrawl"         (show <$> crawlbotMaxToCrawl)
              , mkQuery      "maxToProcess"       (show <$> crawlbotMaxToProcess)
              , mkQueryFalse "restrictDomain"     crawlbotRestrictDomain
              , mkQuery      "notifyEmail" crawlbotNotifyEmail
              , mkQuery      "notifyWebHook"      crawlbotNotifyWebHook
              , mkQuery      "crawlDelay"         (show <$> crawlbotDelay)
              , mkQuery      "repeat"             (show <$> crawlbotRepeat)
              , mkQueryFalse "onlyProcessIfNew"   crawlbotOnlyProcessIfNew
--              , mkQuery      "maxRounds"          (Just $ show crawlbotMaxRounds)  -- test with Nothing when change
              ]


mkQueryApi :: String -> Maybe Req -> Maybe (String, Maybe String)
mkQueryApi name req = mkQuery name (f <$> req)
  where
    f a = reqApi a ++ (BC.unpack . renderQuery True . toQuery $ reqQuery a)


mkQueryLimit :: String -> Maybe Limit -> Maybe (String, Maybe String)
mkQueryLimit name limit = f <$> limit
  where
    f l = case l of
            Pattern s -> ((name ++ "Pattern"), (Just $ unpatternStrings s))
            RegEx s   -> ((name ++ "RegEx"),   (Just s))


unpatternStrings :: [String] -> String
unpatternStrings = intercalate "||"


patternStrings :: String -> [String]
patternStrings "" = []
patternStrings s  = p : patternStrings s'
    where
      (p, s') = split (\(x,y) -> x == y && y == '|') s


split :: ((a, a) -> Bool) -> [a] -> ([a], [a])
split _ []             = ([], [])
split _ (x:[])         = ([x], [])
split p (x:y:xs')
           | p (x, y)  = ([],xs')
           | otherwise = let (ys,zs) = split p (y:xs') in (x:ys, zs)

data Limit
    -- | Specify strings to limit pages to those whose URLs contain
    -- any of the content strings. You can use the exclamation point
    -- to specify a negative string, e.g. \"!product\" to exclude URLs
    -- containing the string \"product\".
    = Pattern [String]
    -- | Specify a regular expression to limit pages to those URLs
    -- that match your expression.
    | RegEx String


data Response = Response
    { responseString :: Maybe String
    , responseJobs   :: [Job]
    } deriving Show


instance FromJSON Response where
    parseJSON (Object v) = Response <$> v .:? "response"
                                    <*> v .:  "jobs"
    parseJSON _          = mzero


data Job = Job
    { jobName                 :: String
    , jobType                 :: String
    , jobStatus               :: Status
    , jobSentDoneNotification :: Int
    , jobObjectsFound         :: Int
    , jobUrlsHarvested        :: Int
    , jobPageCrawlAttempts    :: Int
    , jobPageCrawlSuccesses   :: Int
    , jobPageProcessAttempts  :: Int
    , jobPageProcessSuccesses :: Int
    , jobMaxRounds            :: Int
    , jobRepeat               :: Double
    , jobCrawlDelay           :: Double
    , jobMaxToCrawl           :: Int
    , jobMaxToProcess         :: Int
    , jobObeyRobots           :: Bool
    , jobRestrictDomain       :: Bool
    , jobOnlyProcessIfNew     :: Bool
    , jobSeeds                :: [String]
    , jobRoundsCompleted      :: Int
    , jobRoundStartTime       :: UTCTime
    , jobCurrentTime          :: UTCTime
    , jobApiUrl               :: String
    , jobUrlCrawlPattern      :: [String]
    , jobUrlProcessPattern    :: [String]
    , jobPageProcessPattern   :: [String]
    , jobUrlCrawlRegEx        :: String
    , jobUrlProcessRegEx      :: String
    , jobDownloadJson         :: String
    , jobDownloadUrls         :: String
    , jobNotifyEmail          :: String
    , jobNotifyWebhook        :: String
    } deriving Show


instance FromJSON Job where
    parseJSON (Object v) = Job <$> v .: "name"
                               <*> v .: "type"
                               <*> v .: "jobStatus"
                               <*> v .: "sentJobDoneNotification"
                               <*> v .: "objectsFound"
                               <*> v .: "urlsHarvested"
                               <*> v .: "pageCrawlAttempts"
                               <*> v .: "pageCrawlSuccesses"
                               <*> v .: "pageProcessAttempts"
                               <*> v .: "pageProcessSuccesses"
                               <*> v .: "maxRounds"
                               <*> v .: "repeat"
                               <*> v .: "crawlDelay"
                               <*> v .: "maxToCrawl"
                               <*> v .: "maxToProcess"
                               <*> (toEnum <$> v .: "obeyRobots")
                               <*> (toEnum <$> v .: "restrictDomain")
                               <*> (toEnum <$> v .: "onlyProcessIfNew")
                               <*> (words <$> v .: "seeds")
                               <*> v .: "roundsCompleted"
                               <*> (toTime <$> v .: "roundStartTime")
                               <*> (toTime <$> v .: "currentTime")
                               <*> v .: "apiUrl"
                               <*> (patternStrings <$> v .: "urlCrawlPattern")
                               <*> (patternStrings <$> v .: "urlProcessPattern")
                               <*> (patternStrings <$> v .: "pageProcessPattern")
                               <*> v .: "urlCrawlRegEx"
                               <*> v .: "urlProcessRegEx"
                               <*> v .: "downloadJson"
                               <*> v .: "downloadUrls"
                               <*> v .: "notifyEmail"
                               <*> v .: "notifyWebhook"
    parseJSON _          = mzero


toTime :: Double -> UTCTime
toTime = posixSecondsToUTCTime . realToFrac


data Status = Status
    { statusCode    :: Int
    , statusMessage :: String
    } deriving Show


instance FromJSON Status where
    parseJSON (Object v) = Status <$> v .: "status"
                                  <*> v .: "message"
    parseJSON _          = mzero


data Command = Command
    { commandName   :: String
    -- ^ Job name as defined when the crawl was created.
    , commandAction :: Action
    -- ^ Action to execute.
    }


data Action
    -- | Pause a crawl.
    = Pause
    -- | Resume a paused crawl.
    | Resume
    -- | Restart removes all crawled data while maintaining crawl
    -- settings.
    | Restart
    -- | Delete a crawl, and all associated data, completely.
    | Delete


mkCrawlbot :: String      -- ^ Job name.
           -> Maybe [String]    -- ^ Seed URLs.
           -> Crawlbot
mkCrawlbot name seeds =
    Crawlbot { crawlbotName               = name
             , crawlbotSeeds              = seeds
             , crawlbotApi                = Nothing
             , crawlbotUrlCrawlLimit      = Nothing
             , crawlbotUrlProcessLimit    = Nothing
             , crawlbotPageProcessPattern = Nothing
             , crawlbotMaxToCrawl         = Nothing
             , crawlbotMaxToProcess       = Nothing
             , crawlbotRestrictDomain     = True
             , crawlbotNotifyEmail        = Nothing
             , crawlbotNotifyWebHook      = Nothing
             , crawlbotDelay              = Nothing
             , crawlbotRepeat             = Nothing
             , crawlbotOnlyProcessIfNew   = True
             , crawlbotMaxRounds          = (-1)
             }
