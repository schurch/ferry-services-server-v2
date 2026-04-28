{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Scraper
  ( corranStatusFromPageHtml,
    fetchCorranFerryAndNotify,
    fetchOrkneyFerriesAndNotify,
    fetchPentlandFerriesAndNotify,
    fetchShetlandFerriesAndNotify,
    fetchCalMacStatusesAndNotify,
    fetchNorthLinkServicesAndNotify,
    fetchWesternFerriesAndNotify,
  )
where

import AWS
import Amazonka (AWSRequest (response))
import CMark (commonmarkToHtml)
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch, try)
import Control.Monad (forM_, forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (Value (String), eitherDecode, encode)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char (toLower)
import Data.List (find, isInfixOf, isPrefixOf, nub, sortOn, (\\))
import Data.List.Utils (replace)
import Data.Map (Map, findWithDefault, fromList)
import Data.Maybe (fromJust, fromMaybe, listToMaybe, mapMaybe)
import Data.Pool (Pool, withResource)
import Data.Text (pack, unpack)
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UUID (UUID)
import qualified Database as DB
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest, setRequestBodyJSON, setRequestHeaders, setRequestMethod)
import Network.HTTP.Types.Header
  ( hAccept,
    hAcceptEncoding,
    hAcceptLanguage,
    hConnection,
    hContentType,
    hHost,
    hOrigin,
    hUserAgent,
  )
import System.Directory (doesFileExist, findExecutable)
import System.Exit (ExitCode (..))
import App.Logger (logInfoM)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import Text.HTML.TagSoup (Tag (..), fromAttrib, isTagOpen, isTagOpenName, parseTags, renderTags, (~/=))
import Text.HTML.TagSoup.Tree (renderTree, tagTree)
import Text.Regex (mkRegex, subRegex)
import Types
import Utility (trim)

newtype ScrapedServices = ScrapedServices {unScrapedServices :: [Service]}

newtype DatabaseServices = DatabaseServices {unDatabaseServices :: [Service]}

data CorranNewsItem = CorranNewsItem
  { corranNewsItemTitle :: String,
    corranNewsItemSummary :: String,
    corranNewsItemURL :: String
  }

fetchCorranFerryAndNotify :: Application ()
fetchCorranFerryAndNotify = do
  logInfoM "Fetching Corran Ferry service"
  scrapedService <- liftIO fetchCorranFerry
  let scrapedServices = ScrapedServices [scrapedService]
  databaseServices <- DatabaseServices <$> DB.getServicesForOrganisation 7
  DB.saveServices $ unScrapedServices scrapedServices
  notifyForServices scrapedServices databaseServices

fetchCorranFerry :: IO Service
fetchCorranFerry = do
  time <- getCurrentTime
  return
    Service
      { serviceID = 6000,
        serviceUpdated = time,
        serviceArea = "Corran",
        serviceRoute = "Nether Lochaber - Ardgour",
        serviceStatus = Unknown,
        serviceAdditionalInfo = Nothing,
        serviceDisruptionReason = Nothing,
        serviceOrganisationID = 7,
        serviceLastUpdatedDate = Nothing
      }

fetchCorranFromFacebook :: IO (Maybe (ServiceStatus, String))
fetchCorranFromFacebook = do
  let url = "https://www.facebook.com/CorranFerryService/"
  result <- try @SomeException (fetchCorranFacebookTextWithPlaywright url)
  case result of
    Left _ -> pure Nothing
    Right text ->
      case extractCorranFacebookLatestPost text of
        Just postText ->
          let status = corranFacebookTextToStatus postText
           in if status == Unknown
                then pure Nothing
                else pure (Just (status, corranFacebookPostToHtml url postText))
        Nothing -> pure Nothing

fetchCorranPage :: IO String
fetchCorranPage = do
  let url = "https://www.highland.gov.uk/corran-ferry"
  page <- fetchPage url
  if corranPageHasNewsItems page
    then pure page
    else fetchCorranPageWithPlaywright url

corranPageHasNewsItems :: String -> Bool
corranPageHasNewsItems = not . null . extractCorranNewsItems . parseTags

fetchCorranPageWithPlaywright :: String -> IO String
fetchCorranPageWithPlaywright url = do
  nodeExecutable <- findExecutable "node"
  case nodeExecutable of
    Nothing ->
      error "Corran Playwright fetch required but 'node' is not installed"
    Just nodePath -> do
      scriptPath <- resolveCorranPlaywrightScript "fetch-corran-page.mjs"
      (exitCode, stdoutText, stderrText) <-
        readProcessWithExitCode
          nodePath
          [scriptPath, url]
          ""
      case exitCode of
        ExitSuccess -> pure stdoutText
        ExitFailure _ ->
          error ("Corran Playwright fetch failed: " <> stderrText)

fetchCorranFacebookTextWithPlaywright :: String -> IO String
fetchCorranFacebookTextWithPlaywright url = do
  nodeExecutable <- findExecutable "node"
  case nodeExecutable of
    Nothing ->
      error "Corran Facebook Playwright fetch required but 'node' is not installed"
    Just nodePath -> do
      scriptPath <- resolveCorranPlaywrightScript "fetch-corran-facebook-text.mjs"
      (exitCode, stdoutText, stderrText) <-
        readProcessWithExitCode
          nodePath
          [scriptPath, url]
          ""
      case exitCode of
        ExitSuccess -> pure stdoutText
        ExitFailure _ ->
          error ("Corran Facebook Playwright fetch failed: " <> stderrText)

resolveCorranPlaywrightScript :: FilePath -> IO FilePath
resolveCorranPlaywrightScript scriptName = do
  let candidatePaths =
        [ "/opt/ferry-services/scripts/" <> scriptName,
          "scripts/" <> scriptName
        ]
  existingPath <- findM doesFileExist candidatePaths
  case existingPath of
    Just path -> pure path
    Nothing ->
      error "Corran Playwright script not found"

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = pure Nothing
findM predicate (x : xs) = do
  matches <- predicate x
  if matches
    then pure (Just x)
    else findM predicate xs

corranStatusFromPageHtml :: String -> ServiceStatus
corranStatusFromPageHtml = fst . corranStatusAndNewsItemFromPageHtml

corranStatusAndNewsItemFromPageHtml :: String -> (ServiceStatus, Maybe CorranNewsItem)
corranStatusAndNewsItemFromPageHtml page =
  (maybe Unknown corranNewsItemToStatus selectedNews, selectedNews)
  where
    newsItems = extractCorranNewsItems (parseTags page)
    selectedNews = find isRelevantCorranNewsItem newsItems <|> listToMaybe newsItems

isRelevantCorranNewsItem :: CorranNewsItem -> Bool
isRelevantCorranNewsItem CorranNewsItem {..} =
  let lowerTitle = map toLower corranNewsItemTitle
   in "corran" `isInfixOf` lowerTitle
        && any
          (`isInfixOf` lowerTitle)
          [ "service resumed",
            "service update",
            "passenger service update",
            "repairs update",
            "refit update",
            "outage",
            "withdrawn"
          ]

corranNewsItemToStatus :: CorranNewsItem -> ServiceStatus
corranNewsItemToStatus CorranNewsItem {..} =
  corranTextToStatus (corranNewsItemTitle <> " " <> corranNewsItemSummary)

corranTextToStatus :: String -> ServiceStatus
corranTextToStatus text
  | any (`isInfixOf` lowerText) ["withdrawn from service", "service suspended", "no service", "cancelled", "outage"] = Cancelled
  | any (`isInfixOf` lowerText) ["service update", "passenger service update", "repairs update", "refit update", "disruption", "reduced service"] = Disrupted
  | any (`isInfixOf` lowerText) ["service resumed", "returned to service", "back in operation", "usual seven day-a-week timetable"] = Normal
  | otherwise = Unknown
  where
    lowerText = map toLower text

extractCorranFacebookLatestPost :: String -> Maybe String
extractCorranFacebookLatestPost text =
  firstNonEmpty
    [ extractPostBlock normalisedLines,
      extractPostBlock (dropWhile (not . isLikelyPostPrefix) normalisedLines)
    ]
  where
    normalisedLines =
      filter (not . null) $
        trim <$> lines text

    extractPostBlock :: [String] -> Maybe String
    extractPostBlock [] = Nothing
    extractPostBlock (authorLine : timeLine : dotLine : rest)
      | isCorranAuthor authorLine && isLikelyTimeLine timeLine && dotLine == "\183" =
          let postLines = takeWhile (not . isFacebookPostStopLine) rest
           in if null postLines then Nothing else Just (unlines postLines)
    extractPostBlock (_ : rest) = extractPostBlock rest

    isCorranAuthor :: String -> Bool
    isCorranAuthor line = "corran ferry" == map toLower line

    isLikelyTimeLine :: String -> Bool
    isLikelyTimeLine line =
      any (`isInfixOf` lowerLine) ["h", "hr", "hrs", "hour", "hours", "day", "days", "min", "mins", "minute", "minutes", "am", "pm", "/"]
      where
        lowerLine = map toLower line

    isLikelyPostPrefix :: String -> Bool
    isLikelyPostPrefix line = map toLower line == "corran ferry"

    isFacebookPostStopLine :: String -> Bool
    isFacebookPostStopLine line =
      any (`isInfixOf` lowerLine)
        [ "all reactions:",
          "corran ferry restricted who can comment",
          "see more from corran ferry",
          "email or phone",
          "password",
          "log in",
          "create new account",
          "privacy policy",
          "terms",
          "ad choices",
          "cookie"
        ]
        || line `elem` ["\36096\26377\24515\24773\35746\65306", "\36190", "\35780\35770", "\30331\24405"]
      where
        lowerLine = map toLower line

    firstNonEmpty :: [Maybe String] -> Maybe String
    firstNonEmpty = listToMaybe . mapMaybe id

corranFacebookTextToStatus :: String -> ServiceStatus
corranFacebookTextToStatus text
  | any (`isInfixOf` lowerText) ["cancelled", "no service", "service suspended", "suspended", "withdrawn from service"] = Cancelled
  | any (`isInfixOf` lowerText) ["investigate the issue", "issue on the mv corran", "issue on the", "apologise for any inconvenience", "replacement vessel", "technical issue", "technical issues", "reduced service", "disruption", "delayed", "delay"] = Disrupted
  | any (`isInfixOf` lowerText) ["service resumed", "returned to service", "back in service", "operating normally", "operating as normal", "usual timetable", "usual seven day-a-week timetable"] = Normal
  | otherwise = Unknown
  where
    lowerText = map toLower text

corranFacebookPostToHtml :: String -> String -> String
corranFacebookPostToHtml url postText =
  "<p><a href='" <> url <> "'>Corran Ferry Facebook</a></p>"
    <> concatMap (\paragraph -> "<p>" <> paragraph <> "</p>") (filter (not . null) (trim <$> lines postText))

fetchCorranAdditionalInfo :: CorranNewsItem -> IO String
fetchCorranAdditionalInfo CorranNewsItem {..} = do
  htmlTags <- parseTags <$> fetchPage corranNewsItemURL
  let bodyParagraphs = extractCorranArticleParagraphs htmlTags
      titleHtml = "<p><a href='" <> corranNewsItemURL <> "'>" <> corranNewsItemTitle <> "</a></p>"
      bodyHtml = unwords $ (\paragraph -> "<p>" <> paragraph <> "</p>") <$> bodyParagraphs
  return $ titleHtml <> bodyHtml

extractCorranNewsItems :: [Tag String] -> [CorranNewsItem]
extractCorranNewsItems htmlTags =
  mapMaybe articleToNewsItem (extractCorranListingArticles htmlTags)
  where
    articleToNewsItem :: [Tag String] -> Maybe CorranNewsItem
    articleToNewsItem articleTags = do
      ((href, title), _) <- listToMaybe $ extractTagContent "a" (\tag -> "listing__link" `isInfixOf` fromAttrib "class" tag) articleTags
      let summary = maybe "" snd . listToMaybe $ extractTagContent "p" (\tag -> "listing__summary" `isInfixOf` fromAttrib "class" tag) articleTags
      pure
        CorranNewsItem
          { corranNewsItemTitle = title,
            corranNewsItemSummary = summary,
            corranNewsItemURL =
              if "http" `isInfixOf` href
                then href
                else "https://www.highland.gov.uk" <> href
          }

extractCorranListingArticles :: [Tag String] -> [[Tag String]]
extractCorranListingArticles = go
  where
    go [] = []
    go (tag : rest)
      | isListingArticle tag =
          let (articleTags, remaining) = collectArticle 1 [] rest
           in articleTags : go remaining
      | otherwise = go rest

    isListingArticle :: Tag String -> Bool
    isListingArticle tag =
      isTagOpenName "article" tag && "listing" `isInfixOf` fromAttrib "class" tag

    collectArticle :: Int -> [Tag String] -> [Tag String] -> ([Tag String], [Tag String])
    collectArticle _ acc [] = (reverse acc, [])
    collectArticle depth acc (tag : rest) =
      case tag of
        TagOpen "article" _
          -> collectArticle (depth + 1) (tag : acc) rest
        TagClose "article"
          | depth == 1 -> (reverse acc, rest)
          | otherwise -> collectArticle (depth - 1) (tag : acc) rest
        _ -> collectArticle depth (tag : acc) rest

extractCorranArticleParagraphs :: [Tag String] -> [String]
extractCorranArticleParagraphs htmlTags =
  filter (not . null)
    . map (trim . replace "\160" " ")
    . map snd
    . extractTagContent "p" (const True)
    $ case dropWhile (not . isEditorDiv) htmlTags of
      [] -> []
      TagOpen _ _ : rest -> collectDiv 1 [] rest
      _ -> []
  where
    isEditorDiv :: Tag String -> Bool
    isEditorDiv tag =
      isTagOpenName "div" tag && fromAttrib "class" tag == "editor"

    collectDiv :: Int -> [Tag String] -> [Tag String] -> [Tag String]
    collectDiv _ acc [] = reverse acc
    collectDiv depth acc (tag : rest) =
      case tag of
        TagOpen "div" _ -> collectDiv (depth + 1) (tag : acc) rest
        TagClose "div"
          | depth == 1 -> reverse acc
          | otherwise -> collectDiv (depth - 1) (tag : acc) rest
        _ -> collectDiv depth (tag : acc) rest

extractTagContent :: String -> (Tag String -> Bool) -> [Tag String] -> [((String, String), String)]
extractTagContent tagName predicate = go
  where
    go [] = []
    go (tag : rest)
      | isTagOpenName tagName tag && predicate tag =
          let (content, remaining) = collectTag 1 [] rest
              text = trim $ renderText content
           in ((fromAttrib "href" tag, text), text) : go remaining
      | otherwise = go rest

    collectTag :: Int -> [Tag String] -> [Tag String] -> ([Tag String], [Tag String])
    collectTag _ acc [] = (reverse acc, [])
    collectTag depth acc (tag : rest) =
      case tag of
        TagOpen name _
          | name == tagName -> collectTag (depth + 1) (tag : acc) rest
        TagClose name
          | name == tagName ->
              if depth == 1
                then (reverse acc, rest)
                else collectTag (depth - 1) (tag : acc) rest
        _ -> collectTag depth (tag : acc) rest

    renderText :: [Tag String] -> String
    renderText = concatMap (\t -> case t of TagText text -> text; _ -> "")

fetchOrkneyFerriesAndNotify :: Application ()
fetchOrkneyFerriesAndNotify = do
  logInfoM "Fetching Orkney Ferries services"
  scrapedServices <- liftIO fetchOrkneyFerries
  databaseServices <- DatabaseServices <$> DB.getServicesForOrganisation 5
  DB.saveServices $ unScrapedServices scrapedServices
  notifyForServices scrapedServices databaseServices

fetchPentlandFerriesAndNotify :: Application ()
fetchPentlandFerriesAndNotify = do
  logInfoM "Fetching Pentland Ferries service"
  scrapedServices <- ScrapedServices . (: []) <$> liftIO fetchPentlandFerries
  databaseServices <- DatabaseServices <$> DB.getServicesForOrganisation 6
  DB.saveServices $ unScrapedServices scrapedServices
  notifyForServices scrapedServices databaseServices

fetchPentlandFerries :: IO Service
fetchPentlandFerries = do
  htmlTags <- parseTags <$> fetchPage "https://pentlandferries.co.uk/"
  let announcementLines = extractPentlandAnnouncementLines htmlTags
  time <- getCurrentTime
  return
    Service
      { serviceID = 5000,
        serviceUpdated = time,
        serviceArea = "Pentland Firth",
        serviceRoute = "Gills Bay - St Margaret's Hope",
        serviceStatus = announcementTextToStatus (unwords announcementLines),
        serviceAdditionalInfo = announcementLinesToHtml announcementLines,
        serviceDisruptionReason = Nothing,
        serviceOrganisationID = 6,
        serviceLastUpdatedDate = Nothing
      }
  where
    announcementTextToStatus :: String -> ServiceStatus
    announcementTextToStatus text
      | "cancelled" `isInfixOf` lowerText = Cancelled
      | any (`isInfixOf` lowerText) ["disruption", "delayed", "delay", "amended", "adverse weather", "weather conditions"] = Disrupted
      | otherwise = Normal
      where
        lowerText = map toLower text

    announcementLinesToHtml :: [String] -> Maybe String
    announcementLinesToHtml [] = Nothing
    announcementLinesToHtml lines' = Just $ unwords $ (\line -> "<p>" <> line <> "</p>") <$> lines'

extractPentlandAnnouncementLines :: [Tag String] -> [String]
extractPentlandAnnouncementLines tags =
  case dropWhile (not . isAnnouncementContainer) tags of
    [] -> []
    TagOpen _ _ : rest -> go 1 [] rest
    _ -> []
  where
    isAnnouncementContainer :: Tag String -> Bool
    isAnnouncementContainer tag =
      isTagOpenName "div" tag && "vc_acf announce" `isInfixOf` fromAttrib "class" tag

    go :: Int -> [String] -> [Tag String] -> [String]
    go _ acc [] = reverse acc
    go depth acc (tag : rest) =
      case tag of
        TagOpen "div" _ -> go (depth + 1) acc rest
        TagClose "div"
          | depth == 1 -> reverse acc
          | otherwise -> go (depth - 1) acc rest
        TagText raw ->
          let cleaned = trim (replace "\160" " " raw)
           in if null cleaned
                then go depth acc rest
                else go depth (cleaned : acc) rest
        _ -> go depth acc rest

fetchOrkneyFerries :: IO ScrapedServices
fetchOrkneyFerries = do
  htmlTags <- parseTags <$> fetchPage "https://www.orkneyferries.co.uk/info/current-service-update"
  let disruptionTags = takeWhile (~/= ("<a href=/info/about>" :: String)) . dropWhile (~/= ("<h4>" :: String)) $ htmlTags
  let statuses = map (trim . fromAttrib "src") . filter (isTagOpenName "img") $ disruptionTags
  newsTags <- parseTags <$> fetchPage "https://www.orkneyferries.co.uk/news"
  let additionalInfo =
        "<style>ul>li { margin-bottom: 20px; } ul>li li { margin-bottom: 0px; }</style>"
          <> maybe
            ""
            (replace "\226\128\147" "-" . replace "\226\128\153" "'" . renderTree . (: []))
            (listToMaybe . tagTree . dropWhile (~/= ("<div class=uk-placeholder>" :: String)) $ newsTags)
  time <- liftIO getCurrentTime
  return $
    ScrapedServices
      [ Service
          { serviceID = 4000,
            serviceUpdated = time,
            serviceArea = "Eday",
            serviceRoute = "Kirkwall - Eday - Stronsay - Sanday - Rapness",
            serviceStatus = statusImageTextToStatus $ statuses !! 0,
            serviceAdditionalInfo = Just additionalInfo,
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 5,
            serviceLastUpdatedDate = Nothing
          },
        Service
          { serviceID = 4001,
            serviceUpdated = time,
            serviceArea = "Sanday",
            serviceRoute = "Kirkwall - Eday - Stronsay - Sanday - Rapness",
            serviceStatus = statusImageTextToStatus $ statuses !! 1,
            serviceAdditionalInfo = Just additionalInfo,
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 5,
            serviceLastUpdatedDate = Nothing
          },
        Service
          { serviceID = 4002,
            serviceUpdated = time,
            serviceArea = "Stronsay",
            serviceRoute = "Kirkwall - Eday - Stronsay - Sanday - Rapness",
            serviceStatus = statusImageTextToStatus $ statuses !! 2,
            serviceAdditionalInfo = Just additionalInfo,
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 5,
            serviceLastUpdatedDate = Nothing
          },
        Service
          { serviceID = 4003,
            serviceUpdated = time,
            serviceArea = "Westray",
            serviceRoute = "Kirkwall - Eday - Stronsay - Sanday - Rapness",
            serviceStatus = statusImageTextToStatus $ statuses !! 3,
            serviceAdditionalInfo = Just additionalInfo,
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 5,
            serviceLastUpdatedDate = Nothing
          },
        Service
          { serviceID = 4004,
            serviceUpdated = time,
            serviceArea = "Shapinsay",
            serviceRoute = "Kirkwall - Shapinsay",
            serviceStatus = statusImageTextToStatus $ statuses !! 4,
            serviceAdditionalInfo = Just additionalInfo,
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 5,
            serviceLastUpdatedDate = Nothing
          },
        Service
          { serviceID = 4005,
            serviceUpdated = time,
            serviceArea = "Graemsay",
            serviceRoute = "Stromness - Graemsay - Hoy",
            serviceStatus = statusImageTextToStatus $ statuses !! 5,
            serviceAdditionalInfo = Just additionalInfo,
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 5,
            serviceLastUpdatedDate = Nothing
          },
        Service
          { serviceID = 4006,
            serviceUpdated = time,
            serviceArea = "Houton",
            serviceRoute = "Houton - Flotta - Lyness - Longhope",
            serviceStatus = statusImageTextToStatus $ statuses !! 6,
            serviceAdditionalInfo = Just additionalInfo,
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 5,
            serviceLastUpdatedDate = Nothing
          },
        Service
          { serviceID = 4007,
            serviceUpdated = time,
            serviceArea = "Rousay, Egilsay & Wyre",
            serviceRoute = "Tingwall - Rousay - Egilsay - Wyre",
            serviceStatus = statusImageTextToStatus $ statuses !! 7,
            serviceAdditionalInfo = Just additionalInfo,
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 5,
            serviceLastUpdatedDate = Nothing
          },
        Service
          { serviceID = 4008,
            serviceUpdated = time,
            serviceArea = "Pierowall - Papa Westray",
            serviceRoute = "Westray Pierowall - Papa Westray",
            serviceStatus = statusImageTextToStatus $ statuses !! 8,
            serviceAdditionalInfo = Just additionalInfo,
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 5,
            serviceLastUpdatedDate = Nothing
          }
      ]
  where
    statusImageTextToStatus :: String -> ServiceStatus
    statusImageTextToStatus text
      | "tick" `isInfixOf` text = Normal
      | "warning" `isInfixOf` text = Disrupted
      | "no_entry" `isInfixOf` text = Cancelled
      | otherwise = error $ "Unknown orkney ferries status " <> text

fetchShetlandFerriesAndNotify :: Application ()
fetchShetlandFerriesAndNotify = do
  logInfoM "Fetching Shetland Ferries services"
  scrapedServices <- liftIO fetchShetlandFerries
  databaseServices <- DatabaseServices <$> DB.getServicesForOrganisation 4
  DB.saveServices $ unScrapedServices scrapedServices
  notifyForServices scrapedServices databaseServices

fetchShetlandFerries :: IO ScrapedServices
fetchShetlandFerries = do
  htmlTags <- parseTags <$> fetchPage "https://www.shetland.gov.uk/ferrystatus"
  let disruptionTags = takeWhile (~/= ("</div>" :: String)) . dropWhile (~/= ("<div class=routestatus>" :: String)) $ htmlTags
  let statuses = map (trim . fromAttrib "class") . filter (isTagOpenName "ul") $ disruptionTags
  time <- liftIO getCurrentTime
  return $
    ScrapedServices
      [ Service
          { serviceID = 3000,
            serviceUpdated = time,
            serviceArea = "Bluemull Sound",
            serviceRoute = "Gutcher - Belmont - Hamars Ness",
            serviceStatus = statusTextToStatus $ statuses !! 0,
            serviceAdditionalInfo = Just $ createAdditionalInfo "Bluemull Sound" "01595 743971",
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 4,
            serviceLastUpdatedDate = Nothing
          },
        Service
          { serviceID = 3001,
            serviceUpdated = time,
            serviceArea = "Yell",
            serviceRoute = "Toft - Ulsta",
            serviceStatus = statusTextToStatus $ statuses !! 1,
            serviceAdditionalInfo = Just $ createAdditionalInfo "Yell Sound" "01595 743972",
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 4,
            serviceLastUpdatedDate = Nothing
          },
        Service
          { serviceID = 3003,
            serviceUpdated = time,
            serviceArea = "Whalsay",
            serviceRoute = "Laxo - Symbister",
            serviceStatus = statusTextToStatus $ statuses !! 2,
            serviceAdditionalInfo = Just $ createAdditionalInfo "Whalsay" "01595 743973",
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 4,
            serviceLastUpdatedDate = Nothing
          },
        Service
          { serviceID = 3002,
            serviceUpdated = time,
            serviceArea = "Bressay",
            serviceRoute = "Lerwick - Bressay",
            serviceStatus = statusTextToStatus $ statuses !! 3,
            serviceAdditionalInfo = Just $ createAdditionalInfo "Bressay" "01595 743974",
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 4,
            serviceLastUpdatedDate = Nothing
          },
        Service
          { serviceID = 3004,
            serviceUpdated = time,
            serviceArea = "Skerries",
            serviceRoute = "Laxo - Symbister - Skerries - Vidlin - Lerwick",
            serviceStatus = statusTextToStatus $ statuses !! 4,
            serviceAdditionalInfo = Just $ createAdditionalInfo "Skerries" "01595 743975",
            serviceDisruptionReason = Nothing,
            serviceOrganisationID = 4,
            serviceLastUpdatedDate = Nothing
          }
      ]
  where
    statusTextToStatus :: String -> ServiceStatus
    statusTextToStatus text
      | text == "Route_status_ok" = Normal
      | text == "Route_status_amber" = Disrupted
      | text == "Route_status_red" = Cancelled
      | otherwise = error $ "Unknown shetland ferries status " <> text

    createAdditionalInfo :: String -> String -> String
    createAdditionalInfo serviceName phoneNumber =
      "For more information on the "
        <> serviceName
        <> " service, phone <a='href=tel:"
        <> phoneNumber
        <> "'>"
        <> phoneNumber
        <> "</a>."
        <> "<p>To subscribe to the Shetland Ferries SMS and email alert system please email "
        <> "<a href='mailto:ferries.admin@shetland.gov.uk?subject=SMS%2FEmail%20Subscription&amp;body=Name%3A%0A%0AAddress%3A%0A%0AMobile%20Number%3A%0A%0AEmail%20Address%3A%0A%0ASubscribed%20Routes%20(e.g%20Bluemull%2C%20Yell%2C%20Whalsay%20etc)%3A%0A%0AEmail%20Alerts%20(Yes%2FNo)%3A%0A%0ASMS%20Alerts%20(Yes%2FNo)%3A'>ferries.admin@shetland.gov.uk</a>"
        <> " with your details</p>"

fetchWesternFerriesAndNotify :: Application ()
fetchWesternFerriesAndNotify = do
  logInfoM "Fetching Western Ferries service"
  scrapedServices <- ScrapedServices . (: []) <$> fetchWesternFerries
  databaseServices <- DatabaseServices <$> DB.getServicesForOrganisation 3
  DB.saveServices $ unScrapedServices scrapedServices
  notifyForServices scrapedServices databaseServices

fetchWesternFerries :: Application Service
fetchWesternFerries = do
  htmlTags <- parseTags <$> liftIO (fetchPage "https://status.western-ferries.co.uk/status/view")
  additionalInfo <- liftIO (fetchPage "https://status.western-ferries.co.uk/status/content")
  let activeTag = find (\t -> isTagOpen t && isActiveTag (fromAttrib "class" t)) htmlTags
  let status = textToStatus $ statusClassText <$> activeTag
  time <- liftIO getCurrentTime
  return
    Service
      { serviceID = 2000,
        serviceUpdated = time,
        serviceArea = "Cowal & Dunoon",
        serviceRoute = "McInroy's Point (Gourock) - Hunters Quay (Dunoon)",
        serviceStatus = status,
        serviceAdditionalInfo = if null additionalInfo then Nothing else Just additionalInfo,
        serviceDisruptionReason = Nothing,
        serviceOrganisationID = 3,
        serviceLastUpdatedDate = Nothing
      }
  where
    isActiveTag :: String -> Bool
    isActiveTag = elem "active" . words

    statusClassText :: Tag String -> String
    statusClassText = flip (!!) 1 . words . fromAttrib "class"

    textToStatus :: Maybe String -> ServiceStatus
    textToStatus text
      | text == Just "status-green" = Normal
      | text == Just "status-amber" = Disrupted
      | text == Just "status-red" = Cancelled
      | otherwise = error $ "Unknown western ferries status " <> fromMaybe "" text

fetchNorthLinkServicesAndNotify :: Application ()
fetchNorthLinkServicesAndNotify = do
  logInfoM "Fetching NorthLink service"
  scrapedServices <- ScrapedServices . (: []) <$> fetchNorthLinkService
  databaseServices <- DatabaseServices <$> DB.getServicesForOrganisation 2
  DB.saveServices $ unScrapedServices scrapedServices
  notifyForServices scrapedServices databaseServices

fetchNorthLinkService :: Application Service
fetchNorthLinkService = do
  htmlTags <- parseTags <$> liftIO (fetchPage "https://www.northlinkferries.co.uk/opsnews/")
  let statusText = drop 16 . fromAttrib "class" . (!! 4) . dropWhile (~/= ("<div class=header__action>" :: String)) $ htmlTags
  let additionalInfo = extractNorthLinkOpsNewsHtml htmlTags
  time <- liftIO getCurrentTime
  return
    Service
      { serviceID = 1000,
        serviceUpdated = time,
        serviceArea = "Orkney & Shetland",
        serviceRoute = "Scrabster - Stromness / Aberdeen - Kirkwall - Lerwick",
        serviceStatus = textToStatus statusText,
        serviceAdditionalInfo = if null additionalInfo then Nothing else Just additionalInfo,
        serviceDisruptionReason = Nothing,
        serviceOrganisationID = 2,
        serviceLastUpdatedDate = Nothing
      }
  where
    textToStatus :: String -> ServiceStatus
    textToStatus text
      | text == "service-running" = Normal
      | text == "service-disruptions" = Disrupted
      | otherwise = error $ "Unknown northlink status " <> text

extractNorthLinkOpsNewsHtml :: [Tag String] -> String
extractNorthLinkOpsNewsHtml htmlTags =
  let contentTags = takeWhile (not . isSupportHeading) . dropWhile (not . isNorthLinkOpsNewsStart) $ htmlTags
   in renderTags (sanitizeNorthLinkTags contentTags)
  where
    isNorthLinkOpsNewsStart :: Tag String -> Bool
    isNorthLinkOpsNewsStart (TagText text) =
      let cleaned = trim (replace "\160" " " text)
       in "Please be aware of traffic delays" `isInfixOf` cleaned
            || cleaned == "Pentland Firth Arrivals and Departures"
    isNorthLinkOpsNewsStart _ = False

    isSupportHeading :: Tag String -> Bool
    isSupportHeading (TagText text) = trim text == "Support"
    isSupportHeading _ = False

    sanitizeNorthLinkTags :: [Tag String] -> [Tag String]
    sanitizeNorthLinkTags = snd . foldl step ([], [])

    step :: ([String], [Tag String]) -> Tag String -> ([String], [Tag String])
    step (openTags, acc) tag =
      case tag of
        TagOpen name attrs
          | name `elem` allowedTags -> (name : openTags, acc <> [TagOpen name (filterAllowedAttrs attrs)])
          | otherwise -> (openTags, acc)
        TagClose name
          | name `elem` openTags -> (removeFirst name openTags, acc <> [TagClose name])
          | otherwise -> (openTags, acc)
        TagText raw ->
          let cleaned = trim (replace "\160" " " raw)
           in if null cleaned
                then (openTags, acc)
                else
                  if any (`elem` openTags) ["p", "li", "h2", "h3", "h4", "strong", "em", "a"]
                    then (openTags, acc <> [TagText cleaned])
                    else (openTags, acc <> [TagOpen "p" [], TagText cleaned, TagClose "p"])
        _ -> (openTags, acc)

    allowedTags :: [String]
    allowedTags = ["h2", "h3", "h4", "p", "ul", "ol", "li", "strong", "em", "br", "a"]

    filterAllowedAttrs :: [(String, String)] -> [(String, String)]
    filterAllowedAttrs attrs = filter (\(name, _) -> name == "href") attrs

    removeFirst :: Eq a => a -> [a] -> [a]
    removeFirst _ [] = []
    removeFirst value (x : xs)
      | value == x = xs
      | otherwise = x : removeFirst value xs

fetchPage :: String -> IO String
fetchPage location = do
  request <-
    setRequestHeaders
      [ (hUserAgent, "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.36"),
        (hAccept, "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"),
        (hAcceptLanguage, "en-GB,en;q=0.9"),
        (hAcceptEncoding, "identity"),
        (hConnection, "keep-alive")
      ]
      <$> parseRequest location
  response <- timeout (1000000 * 20) (unpack . TE.decodeUtf8With lenientDecode . getResponseBody <$> httpBS request) -- 20 second timeout
  case response of
    Nothing -> error $ "Error fetching " <> location
    Just result -> return result

fetchCalMacStatusesAndNotify :: Application ()
fetchCalMacStatusesAndNotify = do
  logInfoM "Fetching CalMac services"
  scrapedServices <- liftIO fetchCalMacServices
  databaseServices <- DatabaseServices <$> DB.getServicesForOrganisation 1
  DB.saveServices $ unScrapedServices scrapedServices
  DB.hideServicesWithIDs $ generateRemovedServiceIDs scrapedServices databaseServices
  notifyForServices scrapedServices databaseServices
  where
    generateRemovedServiceIDs :: ScrapedServices -> DatabaseServices -> [Int]
    generateRemovedServiceIDs (ScrapedServices newServices) (DatabaseServices oldServices) =
      let newServiceIDs = serviceID <$> newServices
          oldServiceIDs = serviceID <$> oldServices
       in nub $ oldServiceIDs \\ newServiceIDs

    fetchCalMacServices :: IO ScrapedServices
    fetchCalMacServices = do
      let headers =
            [ (hContentType, "application/json"),
              (hAcceptEncoding, "gzip, deflate"),
              (hAccept, "*/*"),
              (hUserAgent, "Mozilla/5.0 (iPhone; CPU iPhone OS 18_1_1 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Mobile/15E148"),
              (hHost, "apim.calmac.co.uk"),
              ("Sec-Fetch-Site", "cross-site"),
              (hAcceptLanguage, "en-GB,en;q=0.9"),
              ("Sec-Fetch-Mode", "cors"),
              (hOrigin, "capacitor://localhost"),
              (hConnection, "keep-alive"),
              ("Sec-Fetch-Dest", "empty")
            ]
      let requestBodyQuery = "{\n  routes {\n    name\n    id\n    routeCode\n    ports {\n      portCode\n      name\n      order\n      isFreight\n      hideOnMap\n      __typename\n    }\n    routeStatuses {\n      id\n      title\n      status\n      subStatus\n      title\n      startDateTime\n      endDateTime\n      detail\n      nextReviewDateTime\n      updatedAtDateTime\n      __typename\n    }\n    location {\n      name\n      __typename\n    }\n    status\n    isFreight\n    hideOnMap\n    __typename\n  }\n}"
      request <- setRequestBodyJSON (CalMacAPIRequestBody requestBodyQuery) . setRequestMethod "POST" . setRequestHeaders headers <$> parseRequest "https://apim.calmac.co.uk/graphql"
      responseBody <-
        checkResponseBody
          <$> timeout (1000000 * 20) (C.fromStrict . getResponseBody <$> httpBS request) -- 20 second timeout
      time <- getCurrentTime
      let result = do
            response <- responseBody >>= eitherDecode
            let routes = calMacAPIResponseDataRoutes . calMacAPIResponseData $ response
            Right $ calmacRouteToService time <$> routes
      case result of
        Left errorMessage -> error errorMessage
        Right result' -> return $ ScrapedServices result'

    checkResponseBody :: Maybe a -> Either String a
    checkResponseBody =
      maybe (Left "Timeout while waiting for services response") Right

calmacRouteToService :: UTCTime -> CalMacAPIResponseRoute -> Service
calmacRouteToService time CalMacAPIResponseRoute {..} =
  Service
    { serviceID = findWithDefault (read calMacAPIResponseRouteRouteCode) calMacAPIResponseRouteRouteCode serviceIDLookup,
      serviceUpdated = time,
      serviceArea = calMacAPIResponseRouteLocationName calMacAPIResponseRouteLocation,
      serviceRoute = cleanupRouteText calMacAPIResponseRouteName,
      serviceStatus = statusToServiceStatus calMacAPIResponseRouteStatus,
      serviceAdditionalInfo = Just $ routeStatusesToAdditionalInfo calMacAPIResponseRouteRouteStatuses,
      serviceDisruptionReason = Nothing,
      serviceOrganisationID = 1,
      serviceLastUpdatedDate = Nothing
    }
  where
    statusToServiceStatus :: String -> ServiceStatus
    statusToServiceStatus status
      | status == "NORMAL" = Normal
      | status == "BE_AWARE" = Disrupted
      | status == "DISRUPTIONS" = Disrupted
      | status == "ALL_SAILINGS_CANCELLED" = Cancelled
      | otherwise = error $ "Unknown calmac status " <> status

    cleanupRouteText :: String -> String
    cleanupRouteText = replace "[" "(" . replace "]" ")" . replace "�" "-"

    routeStatusesToAdditionalInfo :: [CalMacAPIResponseRouteStatus] -> String
    routeStatusesToAdditionalInfo statuses = unwords (statusToHTML <$> sortStatuses statuses)

    sortStatuses :: [CalMacAPIResponseRouteStatus] -> [CalMacAPIResponseRouteStatus]
    sortStatuses statuses =
      sortOn calMacAPIResponseRouteStatusTitle (filterStatuses "SAILING" statuses)
        <> sortOn calMacAPIResponseRouteStatusTitle (filterStatuses "SERVICE" statuses)
        <> sortOn calMacAPIResponseRouteStatusTitle (filterStatuses "INFORMATION" statuses)

    filterStatuses :: String -> [CalMacAPIResponseRouteStatus] -> [CalMacAPIResponseRouteStatus]
    filterStatuses status = filter (\s -> calMacAPIResponseRouteStatusStatus s == status)

    statusToHTML :: CalMacAPIResponseRouteStatus -> String
    statusToHTML CalMacAPIResponseRouteStatus {..} =
      "<h2>"
        <> calMacAPIResponseRouteStatusTitle
        <> "</h2>"
        <> "<p>"
        <> unpack (commonmarkToHtml [] (pack calMacAPIResponseRouteStatusDetail))
        <> "</p>"

    -- Try and map the calmac status code to the old service ids
    serviceIDLookup :: Map String Int
    serviceIDLookup =
      fromList
        [ ("001", 1),
          ("007", 2),
          ("002", 3),
          ("006", 4),
          ("003", 5),
          ("004", 6),
          ("005", 7),
          ("060", 8),
          ("030", 9),
          ("053", 10),
          ("031", 11),
          ("036", 12),
          ("056", 13),
          ("054", 14),
          ("055", 15),
          ("052", 16),
          ("061", 17),
          ("033", 18),
          ("051", 19),
          ("032", 20),
          ("080", 21),
          ("022", 22),
          ("065", 23),
          ("034", 24),
          ("035", 25),
          ("300", 35),
          ("038", 37),
          ("043", 38),
          ("011", 39),
          ("301", 41)
        ]

notifyForServices :: ScrapedServices -> DatabaseServices -> Application ()
notifyForServices (ScrapedServices newServices) (DatabaseServices oldServices) = do
  logger <- asks logger
  forM_ newServices $ \service -> do
    let oldService = find (\s -> serviceID s == serviceID service) oldServices
    case oldService of
      Just oldService -> do
        let statusesDifferent =
              serviceStatus service /= serviceStatus oldService
        let statusValid = serviceStatus service /= Unknown
        let shouldNotify = statusesDifferent && statusValid
        when shouldNotify $ do
          interestedInstallations <- DB.getIntererestedInstallationsForServiceID $ serviceID service
          let defaultNotificationMessage = serviceToDefaultNotificationMessage service
          let iOSInterestedInstallations =
                filter
                  ((==) IOS . installationDeviceType)
                  interestedInstallations
          let (iosTitle, iosBody) = serviceToIOSNotificationMessage service
          forM_ iOSInterestedInstallations $
            \Installation {installationID = installationID, installationEndpointARN = endpointARN} ->
              do
                let payload = createApplePushPayload defaultNotificationMessage iosTitle iosBody (serviceID service)
                sendNotification installationID endpointARN payload

          let androidInterestedInstallations =
                filter
                  ((==) Android . installationDeviceType)
                  interestedInstallations
          let (androidTitle, androidBody) = serviceToAndroidNotificationMessage service
          forM_ androidInterestedInstallations $
            \Installation {installationID = installationID, installationEndpointARN = endpointARN} ->
              do
                let payload = createAndroidPushPayload defaultNotificationMessage androidTitle androidBody (serviceID service)
                sendNotification installationID endpointARN payload
      Nothing -> return ()
  where
    sendNotification :: UUID -> String -> PushPayload -> Application ()
    sendNotification installationID endpointARN payload = do
      logger <- asks logger
      result <- liftIO $ sendNotificationWihPayload logger endpointARN payload
      case result of
        SendNotificationEndpointDisabled -> do
          void $ DB.deleteInstallationWithID installationID
          liftIO $ deletePushEndpoint logger endpointARN
        SendNotificationResultSuccess -> return ()

    serviceToDefaultNotificationMessage :: Service -> String
    serviceToDefaultNotificationMessage Service {serviceRoute = serviceRoute, serviceStatus = serviceStatus}
      | serviceStatus == Normal =
          "Normal services have resumed for " <> serviceRoute
      | serviceStatus == Disrupted =
          "There is a disruption to the service " <> serviceRoute
      | serviceStatus == Cancelled =
          "Sailings have been cancelled for " <> serviceRoute
      | serviceStatus == Unknown =
          error "Do not message for unknow service"

    serviceToIOSNotificationMessage :: Service -> (String, String)
    serviceToIOSNotificationMessage Service {serviceArea = serviceArea, serviceRoute = serviceRoute, serviceStatus = serviceStatus}
      | serviceStatus == Normal =
          (serviceArea, "Normal services have resumed for " <> serviceRoute)
      | serviceStatus == Disrupted =
          (serviceArea, "There is a disruption to the service " <> serviceRoute)
      | serviceStatus == Cancelled =
          (serviceArea, "Sailings have been cancelled for " <> serviceRoute)
      | serviceStatus == Unknown =
          error "Do not message for unknow service"

    serviceToAndroidNotificationMessage :: Service -> (String, String)
    serviceToAndroidNotificationMessage Service {serviceArea = serviceArea, serviceRoute = serviceRoute, serviceStatus = serviceStatus}
      | serviceStatus == Normal =
          (serviceArea <> " sailings resumed", serviceRoute)
      | serviceStatus == Disrupted =
          (serviceArea <> " sailings disrupted", serviceRoute)
      | serviceStatus == Cancelled =
          (serviceArea <> " sailings cancelled", serviceRoute)
      | serviceStatus == Unknown =
          error "Do not message for unknow service"

    createApplePushPayload :: String -> String -> String -> Int -> PushPayload
    createApplePushPayload defaultMessage title body serviceID =
      let apsPayload =
            APSPayload
              ( APSPayloadBody
                  { apsPayloadBodyAlert =
                      APSPayloadBodyAlert
                        { apsPayloadBodyAlertTitle = title,
                          apsPayloadBodyAlertBody = body
                        },
                    apsPayloadBodySound = "default"
                  }
              )
              serviceID
       in PushPayload defaultMessage (ApplePayload apsPayload)

    createAndroidPushPayload :: String -> String -> String -> Int -> PushPayload
    createAndroidPushPayload defaultMessage title body serviceID =
      PushPayload defaultMessage (GooglePayload (GCMPayload (GCMPayloadData serviceID title body) "high" (GCMPayloadAndroid "high")))
