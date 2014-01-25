{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The Crawlbot API allows you to programmatically manage Crawlbot
-- [1] crawls and retrieve output.
--
-- \[1\] <http://diffbot.com/dev/crawl/v2>

module Diffbot.Crawlbot
    (
    -- * Examples
    -- $examples

    -- * Retrieving Crawl Data
    -- $retrieving

    -- * Request
      crawlbot
    , Command(..)
    , Crawl(..)
    , defCrawl
    , Limit(..)
    -- * Response
    , Response(..)
    , Job(..)
    , JobStatus(..)
    ) where

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

import Diffbot.Types
import Diffbot.Internal
import Diffbot.Article (Article)


-- | Manage crawls.
crawlbot :: String    -- ^ Developer token.
         -> Command   -- ^ Action to execute.
         -> IO (Maybe Response)
crawlbot token command =
    bot command [("token", Just token)]


-- | For most commands you should specify a job name as defined when
-- the crawl was created.
data Command
    -- | Create a crawl.
    = Create Crawl
    -- | Get current crawls.
    | List
    -- | Retrieve a single crawl's details.
    | Show String
    -- | Pause a crawl.
    | Pause String
    -- | Resume a paused crawl.
    | Resume String
    -- | Restart removes all crawled data while maintaining crawl
    -- settings.
    | Restart String
    -- | Delete a crawl, and all associated data, completely.
    | Delete String


instance Request Command where
    toReq a = Req { reqApi     = "http://api.diffbot.com/v2/crawl"
                  , reqContent = Nothing
                  , reqQuery   = mkCommandQuery a
                  }


mkCommandQuery :: Command -> [(String, Maybe String)]
mkCommandQuery command =
    case command of
      Create c     -> mkCrawlQuery c
      List         -> []
      Show    name -> [("name", Just name)]
      Pause   name -> [("name", Just name), ("pause",   Just "1")]
      Resume  name -> [("name", Just name), ("pause",   Just "0")]
      Restart name -> [("name", Just name), ("restart", Just "1")]
      Delete  name -> [("name", Just name), ("delete",  Just "1")]


data Crawl = Crawl
    { crawlName               :: String
    -- ^ Should be a unique identifier and can be used to modify your
    -- crawl or retrieve its output.
    , crawlSeeds              :: [String]
    -- ^ Seed URL(s). By default Crawlbot will spider subdomains
    -- (e.g., a seed URL of <http://www.diffbot.com> will include URLs
    -- at <http://blog.diffbot.com>).
    , crawlApi                :: Maybe Req
    -- ^ Diffbot API through which to process pages. E.g., @(Just $
    -- toReq Article)@ to process matching links via the 'Article' API.
    , crawlUrlCrawlLimit      :: Maybe Limit
    -- ^ Limit crawled pages.
    , crawlUrlProcessLimit    :: Maybe Limit
    -- ^ Limit processed pages.
    , crawlPageProcessPattern :: Maybe [String]
    -- ^ Specify strings to limit pages processed to those whose HTML
    -- contains any of the content strings.
    , crawlMaxToCrawl         :: Maybe Int
    -- ^ Specify max pages to spider.
    , crawlMaxToProcess       :: Maybe Int
    -- ^ Specify max pages to process through Diffbot APIs. Default:
    -- 10,000.
    , crawlRestrictDomain     :: Maybe Bool
    -- ^ By default crawls will restrict to subdomains within the seed
    -- URL domain. Specify to @(Just False)@ to follow all links
    -- regardless of domain.
    , crawlNotifyEmail        :: Maybe String
    -- ^ Send a message to this email address when the crawl hits the
    -- 'crawlMaxToCrawl' or 'crawlMaxToProcess' limit, or when
    -- the crawl completes.
    , crawlNotifyWebHook      :: Maybe String
    -- ^ Pass a URL to be notified when the crawl hits the
    -- 'crawlMaxToCrawl' or 'crawlMaxToProcess' limit, or when
    -- the crawl completes. You will receive a POST with X-Crawl-Name
    -- and X-Crawl-Status in the headers, and the full JSON response
    -- in the POST body.
    , crawlDelay              :: Maybe Double
    -- ^ Wait this many seconds between each URL crawled from a single
    -- IP address.
    , crawlRepeat             :: Maybe Double
    -- ^ Specify the number of days to repeat this crawl. By default
    -- crawls will not be repeated.
    , crawlOnlyProcessIfNew   :: Maybe Bool
    -- ^ By default repeat crawls will only process new (previously
    -- unprocessed) pages. Set to @(Just False)@ to process all
    -- content on repeat crawls.
    , crawlMaxRounds          :: Maybe Int
    -- ^ Specify the maximum number of crawl repeats. By default
    -- repeating crawls will continue indefinitely.
    }


mkCrawlQuery :: Crawl -> [(String, Maybe String)]
mkCrawlQuery (Crawl {..}) =
    catMaybes [ mkQuery      "name"               (Just crawlName)
              , mkQuery      "seeds"              (Just $ unwords crawlSeeds)
              , mkQueryApi   "apiUrl"             crawlApi
              , mkQueryLimit "urlCrawl"           crawlUrlCrawlLimit
              , mkQueryLimit "urlProcess"         crawlUrlProcessLimit
              , mkQuery      "pageProcessPattern" (unpatternStrings <$> crawlPageProcessPattern)
              , mkQuery      "maxToCrawl"         (show <$> crawlMaxToCrawl)
              , mkQuery      "maxToProcess"       (show <$> crawlMaxToProcess)
              , mkQuery      "restrictDomain"     (show . fromEnum <$> crawlRestrictDomain)
              , mkQuery      "notifyEmail"        crawlNotifyEmail
              , mkQuery      "notifyWebHook"      crawlNotifyWebHook
              , mkQuery      "crawlDelay"         (show <$> crawlDelay)
              , mkQuery      "repeat"             (show <$> crawlRepeat)
              , mkQuery      "onlyProcessIfNew"   (show . fromEnum <$> crawlOnlyProcessIfNew)
              , mkQuery      "maxRounds"          (show <$> crawlMaxRounds)
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
    -- ^ Response message, e.g. \"Successfully added urls for
    -- spidering.\"
    , responseJobs   :: Maybe [Job]
    -- ^ Full crawl details.
    } deriving Show


instance FromJSON Response where
    parseJSON (Object v) = Response <$> v .:? "response"
                                    <*> v .:? "jobs"
    parseJSON _          = mzero


data Job = Job
    { jobName                 :: String
    , jobType                 :: String
    , jobStatus               :: JobStatus
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

-- | Existing status codes and associated messages:
--
--     * 0 - Job is initializing
--
--     * 1 - Job has reached maxRounds limit
--
--     * 2 - Job has reached maxToCrawl limit
--
--     * 3 - Job has reached maxToProcess limit
--
--     * 4 - Next round to start in _____ seconds
--
--     * 5 - No URLs were added to the crawl
--
--     * 6 - Job paused
--
--     * 7 - Job in progress
--
--     * 9 - Job has completed and no repeat is scheduled
data JobStatus = JobStatus
    { jobStatusCode    :: Int
    , jobStatusMessage :: String
    } deriving Show


instance FromJSON JobStatus where
    parseJSON (Object v) = JobStatus <$> v .: "status"
                                     <*> v .: "message"
    parseJSON _          = mzero


defCrawl :: String      -- ^ Crawl name.
         -> [String]    -- ^ Seed URLs.
         -> Crawl
defCrawl name seeds =
    Crawl { crawlName               = name
          , crawlSeeds              = seeds
          , crawlApi                = Nothing
          , crawlUrlCrawlLimit      = Nothing
          , crawlUrlProcessLimit    = Nothing
          , crawlPageProcessPattern = Nothing
          , crawlMaxToCrawl         = Nothing
          , crawlMaxToProcess       = Nothing
          , crawlRestrictDomain     = Nothing
          , crawlNotifyEmail        = Nothing
          , crawlNotifyWebHook      = Nothing
          , crawlDelay              = Nothing
          , crawlRepeat             = Nothing
          , crawlOnlyProcessIfNew   = Nothing
          , crawlMaxRounds          = Nothing
          }

-- $examples
--
-- To create or update a crawl:
--
-- > import Diffbot
-- > import Diffbot.Crawlbot
-- >
-- > main = do
-- >     let token = "11111111111111111111111111111111"
-- >         crawl = defaultCrawl "sampleDiffbotCrawl" ["http://blog.diffbot.com"]
-- >     resp <- crawlbot token $ Create crawl
-- >     print resp
--
-- To pause, resume, restart or delete crawl you should specify a job
-- name as defined when the crawl was created:
--
-- > main = do
-- >     let token = "11111111111111111111111111111111"
-- >     resp <- crawlbot token $ Pause "sampleDiffbotCrawl"
-- >     print resp


-- $retrieving
--
-- To download results please make a GET request to the following
-- URLs, replacing \"token\" and \"crawlName\" with your token and
-- crawl name, respectively. These are also available in the response,
-- as 'jobDownloadJson' and 'jobDownloadUrls'.
--
-- Download all JSON objects (as processed by Diffbot APIs):
--
-- http:\/\/api.diffbot.com\/v2\/crawl\/download\/\<token\>-\<crawlName\>_data.json
--
-- Download a comma-separated values (CSV) file of URLs encountered by
-- Crawlbot (useful for debugging):
--
-- http:\/\/api.diffbot.com\/v2\/crawl\/download\/\<token\>-\<crawlName\>_urls.csv
