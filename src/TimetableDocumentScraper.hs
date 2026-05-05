{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module TimetableDocumentScraper
  ( fetchTimetableDocuments,
  )
where

import App.Env (Application)
import App.Logger (logErrorM, logInfoM)
import Control.Exception (SomeException, try)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import qualified Crypto.Hash as Crypto
import Data.Aeson
  ( FromJSON (parseJSON),
    Options (fieldLabelModifier),
    Value (Object),
    defaultOptions,
    eitherDecode,
    genericParseJSON,
    object,
    (.=),
  )
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char (isAlphaNum, toLower)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, nubBy)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.Text.Encoding as TE
import qualified Database as DB
import GHC.Generics (Generic)
import Network.HTTP.Simple
  ( getResponseBody,
    getResponseHeaders,
    httpBS,
    httpLBS,
    parseRequest,
    setRequestBodyJSON,
    setRequestHeaders,
    setRequestMethod,
  )
import Network.HTTP.Types.Header
  ( hAccept,
    hAcceptEncoding,
    hAcceptLanguage,
    hConnection,
    hContentLength,
    hContentType,
    hOrigin,
    hUserAgent,
    RequestHeaders,
  )
import System.Timeout (timeout)
import Text.HTML.TagSoup
  ( Tag (..),
    fromAttrib,
    isTagOpenName,
    parseTags,
  )
import Types (ScrapedTimetableDocument (..))
import Utility (trim)

data TimetableDocumentSource = TimetableDocumentSource
  { timetableDocumentSourceOrganisationID :: Int,
    timetableDocumentSourceServiceIDs :: [Int],
    timetableDocumentSourcePageURL :: String,
    timetableDocumentSourceTitlePrefix :: Maybe String
  }

data DocumentLink = DocumentLink
  { documentLinkTitle :: String,
    documentLinkURL :: String
  }

data DocumentMetadata = DocumentMetadata
  { documentMetadataContentHash :: Maybe String,
    documentMetadataContentType :: Maybe String,
    documentMetadataContentLength :: Maybe Int
  }

data CalMacTimetablesResponse = CalMacTimetablesResponse
  { calMacTimetablesResponseData :: CalMacTimetablesData
  }
  deriving (Generic, Show)

instance FromJSON CalMacTimetablesResponse where
  parseJSON = genericParseJSON $ calMacTimetableJsonOptions "calMacTimetablesResponse"

data CalMacTimetablesData = CalMacTimetablesData
  { calMacTimetablesDataTimetables :: [CalMacTimetable]
  }
  deriving (Generic, Show)

instance FromJSON CalMacTimetablesData where
  parseJSON = genericParseJSON $ calMacTimetableJsonOptions "calMacTimetablesData"

data CalMacTimetable = CalMacTimetable
  { calMacTimetableTimetableType :: Maybe String,
    calMacTimetableTitle :: String,
    calMacTimetableRoute :: CalMacTimetableRoute,
    calMacTimetableReleaseDetail :: Maybe String,
    calMacTimetablePdfUrl :: Maybe String,
    calMacTimetableValidFrom :: Maybe String,
    calMacTimetableValidUntil :: Maybe String,
    calMacTimetableLastUpdated :: Maybe String
  }
  deriving (Generic, Show)

instance FromJSON CalMacTimetable where
  parseJSON = genericParseJSON $ calMacTimetableJsonOptions "calMacTimetable"

data CalMacTimetableRoute = CalMacTimetableRoute
  { calMacTimetableRouteName :: String
  }
  deriving (Generic, Show)

instance FromJSON CalMacTimetableRoute where
  parseJSON = genericParseJSON $ calMacTimetableJsonOptions "calMacTimetableRoute"

calMacTimetableJsonOptions :: String -> Options
calMacTimetableJsonOptions prefix =
  jsonOptions
    { fieldLabelModifier = toLowerFirstLetter . drop (length prefix)
    }
  where
    jsonOptions = defaultOptions

fetchTimetableDocuments :: Application ()
fetchTimetableDocuments = do
  documents <- scrapeTimetableDocuments
  logInfoM $ "Saving " <> show (length documents) <> " timetable documents"
  DB.saveTimetableDocuments documents

scrapeTimetableDocuments :: Application [ScrapedTimetableDocument]
scrapeTimetableDocuments = do
  now <- liftIO getCurrentTime
  calMacDocuments <- scrapeCalMacTimetableDocuments now
  nestedDocuments <- forM timetableDocumentSources $ \source -> do
    let sourceLabel = timetableDocumentSourceLabel source
    logInfoM $ "Fetching timetable document source: " <> sourceLabel
    pageResult <- liftIO $ try @SomeException $ fetchPage (timetableDocumentSourcePageURL source)
    case pageResult of
      Left exception -> do
        logErrorM $ "Failed to fetch timetable document source: " <> sourceLabel <> " - " <> show exception
        pure []
      Right page -> do
        let links =
              filterTimetableLinks $
                normalizeDocumentLink (timetableDocumentSourcePageURL source) (timetableDocumentSourceTitlePrefix source)
                  <$> extractDocumentLinks page
        logInfoM $ "Found " <> show (length links) <> " timetable document links for source: " <> sourceLabel
        forM links $ \DocumentLink {..} -> do
          metadata <- fetchDocumentMetadata sourceLabel documentLinkURL
          pure
            ScrapedTimetableDocument
              { scrapedTimetableDocumentOrganisationID = timetableDocumentSourceOrganisationID source,
                scrapedTimetableDocumentServiceIDs = timetableDocumentSourceServiceIDs source,
                scrapedTimetableDocumentTitle = documentLinkTitle,
                scrapedTimetableDocumentSourceURL = documentLinkURL,
                scrapedTimetableDocumentContentHash = documentMetadataContentHash metadata,
                scrapedTimetableDocumentContentType = documentMetadataContentType metadata,
                scrapedTimetableDocumentContentLength = documentMetadataContentLength metadata,
                scrapedTimetableDocumentLastSeenAt = now
              }
  let documents = nubBy sameSourceURL $ calMacDocuments <> concat nestedDocuments
  logInfoM $ "Found " <> show (length documents) <> " unique timetable documents"
  pure documents
  where
    sameSourceURL a b =
      scrapedTimetableDocumentSourceURL a == scrapedTimetableDocumentSourceURL b

scrapeCalMacTimetableDocuments :: UTCTime -> Application [ScrapedTimetableDocument]
scrapeCalMacTimetableDocuments now = do
  logInfoM "Fetching CalMac timetable documents from GraphQL"
  result <- liftIO $ try @SomeException fetchCalMacTimetables
  case result of
    Left exception -> do
      logErrorM $ "Failed to fetch CalMac timetable documents from GraphQL - " <> show exception
      pure []
    Right timetables -> do
      documents <- fmap catMaybes $
        forM timetables $ \timetable@CalMacTimetable {..} ->
          case calMacTimetablePdfUrl of
            Nothing -> pure Nothing
            Just pdfUrl -> case calMacTimetableServiceIDs timetable of
              [] -> do
                logErrorM $ "Skipping unmapped CalMac timetable route: " <> calMacTimetableRouteName calMacTimetableRoute <> " - " <> pdfUrl
                pure Nothing
              serviceIDs -> do
                metadata <- fetchDocumentMetadata "CalMac GraphQL" pdfUrl
                pure $
                  Just
                    ScrapedTimetableDocument
                      { scrapedTimetableDocumentOrganisationID = 1,
                        scrapedTimetableDocumentServiceIDs = serviceIDs,
                        scrapedTimetableDocumentTitle = calMacTimetableDocumentTitle timetable,
                        scrapedTimetableDocumentSourceURL = pdfUrl,
                        scrapedTimetableDocumentContentHash = documentMetadataContentHash metadata,
                        scrapedTimetableDocumentContentType = documentMetadataContentType metadata,
                        scrapedTimetableDocumentContentLength = documentMetadataContentLength metadata,
                        scrapedTimetableDocumentLastSeenAt = now
                      }
      logInfoM $ "Found " <> show (length documents) <> " CalMac timetable documents from GraphQL"
      pure documents

fetchCalMacTimetables :: IO [CalMacTimetable]
fetchCalMacTimetables = do
  request <- setRequestBodyJSON calMacTimetablesRequestBody . setRequestMethod "POST" . setRequestHeaders calMacGraphQLHeaders <$> parseRequest "https://apim.calmac.co.uk/graphql"
  responseBody <-
    checkResponseBody
      <$> timeout requestTimeoutMicros (C.fromStrict . getResponseBody <$> httpBS request)
  case responseBody >>= eitherDecode of
    Left errorMessage -> error errorMessage
    Right response -> pure $ calMacTimetablesDataTimetables . calMacTimetablesResponseData $ response
  where
    checkResponseBody =
      maybe (Left "Timed out fetching CalMac timetable documents") Right

calMacTimetablesRequestBody :: Value
calMacTimetablesRequestBody =
  object
    [ "variables" .= Object mempty,
      "query" .= calMacTimetablesQuery
    ]

calMacTimetablesQuery :: String
calMacTimetablesQuery =
  "{\n  timetables {\n    timetableType\n    title\n    route {\n      name\n      id\n      originPort {\n        name\n        __typename\n      }\n      destinationPort {\n        name\n        __typename\n      }\n      __typename\n    }\n    releaseDetail\n    pdfUrl\n    imageUrl\n    key\n    validFrom\n    validUntil\n    lastUpdated\n    __typename\n  }\n}"

calMacGraphQLHeaders :: RequestHeaders
calMacGraphQLHeaders =
  [ (hContentType, "application/json"),
    (hAccept, "*/*"),
    ("Sec-Fetch-Site", "cross-site"),
    (hAcceptEncoding, "gzip, deflate, br"),
    (hAcceptLanguage, "en-GB,en;q=0.9"),
    ("Sec-Fetch-Mode", "cors"),
    (hOrigin, "capacitor://localhost"),
    (hUserAgent, "Mozilla/5.0 (iPhone; CPU iPhone OS 18_7 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Mobile/15E148"),
    (hConnection, "keep-alive"),
    ("Sec-Fetch-Dest", "empty")
  ]

calMacTimetableServiceIDs :: CalMacTimetable -> [Int]
calMacTimetableServiceIDs CalMacTimetable {..} =
  fromMaybe [] $ M.lookup (normalizeCalMacRouteName $ calMacTimetableRouteName calMacTimetableRoute) calMacTimetableServiceIDLookup

calMacTimetableDocumentTitle :: CalMacTimetable -> String
calMacTimetableDocumentTitle CalMacTimetable {..} =
  calMacTimetableRouteName calMacTimetableRoute
    <> ": "
    <> calMacTimetableTitle
    <> validRange
  where
    validRange =
      case (calMacTimetableValidFrom, calMacTimetableValidUntil) of
        (Just validFrom, Just validUntil) -> " (" <> dateOnly validFrom <> " to " <> dateOnly validUntil <> ")"
        _ -> ""

calMacTimetableServiceIDLookup :: M.Map String [Int]
calMacTimetableServiceIDLookup =
  M.fromList $
    fmap
      (\(routeName, serviceIDs) -> (normalizeCalMacRouteName routeName, serviceIDs))
      [ ("Ardrossan - Brodick", [5]),
        ("Troon - Brodick", [41]),
        ("Claonaig - Lochranza", [6]),
        ("Tarbert (Loch Fyne) - Lochranza (Seasonal Winter)", [6]),
        ("Colintraive - Rhubodach", [4]),
        ("Wemyss Bay - Rothesay", [3]),
        ("Gourock - Dunoon", [1]),
        ("Tarbert (Loch Fyne) - Portavadie", [2]),
        ("Largs - Cumbrae Slip (Millport)", [7]),
        ("Gourock - Kilcreggan", [39]),
        ("Ardrossan - Campbeltown", [36]),
        ("Kennacraig - Port Askaig (Islay) / Port Ellen (Islay)", [9]),
        ("Kennacraig - Islay/C'say/Oban", [9, 10]),
        ("Tayinloan - Gigha", [8]),
        ("Oban - Colonsay - Port Askaig - Kennacraig", [10]),
        ("Tobermory - Kilchoan", [14]),
        ("Fionnphort - Iona", [13]),
        ("Gallanach-Kerrera", [38]),
        ("Oban - Coll/Tiree", [16]),
        ("Oban - Craignure", [11]),
        ("Lochaline - Fishnish", [12]),
        ("Oban - Lismore", [15]),
        ("Mallaig - Eigg/Muck/Rum/Canna", [19]),
        ("Mallaig - Armadale", [18]),
        ("Sconser - Raasay", [17]),
        ("Ardmhor (Barra) - Eriskay", [21]),
        ("Oban - Castlebay", [20]),
        ("Berneray - Leverburgh", [23]),
        ("Uig - Lochmaddy", [22]),
        ("Uig - Tarbert", [24]),
        ("Ullapool - Stornoway", [25]),
        ("Mallaig / Oban - Lochboisdale", [37])
      ]

normalizeCalMacRouteName :: String -> String
normalizeCalMacRouteName =
  unwords . words . lower . replaceAll "–" "-" . replaceAll "\160" " "

dateOnly :: String -> String
dateOnly = takeWhile (/= 'T')

toLowerFirstLetter :: String -> String
toLowerFirstLetter [] = []
toLowerFirstLetter (x : xs) = toLower x : xs

timetableDocumentSourceLabel :: TimetableDocumentSource -> String
timetableDocumentSourceLabel TimetableDocumentSource {..} =
  fromMaybe timetableDocumentSourcePageURL timetableDocumentSourceTitlePrefix

timetableDocumentSources :: [TimetableDocumentSource]
timetableDocumentSources =
  [ TimetableDocumentSource 2 [1000] "https://www.northlinkferries.co.uk/timetables/" (Just "NorthLink"),
    TimetableDocumentSource 3 [2000] "https://western-ferries.co.uk/timetables/" (Just "Western Ferries"),
    TimetableDocumentSource 5 [4000, 4001, 4002, 4003, 4004, 4005, 4006, 4007, 4008] "https://www.orkneyferries.co.uk/timetables" (Just "Orkney Ferries"),
    TimetableDocumentSource 4 [3000, 3001, 3002, 3003, 3004] "https://www.shetland.gov.uk/ferries/timetable" (Just "Shetland Ferries"),
    TimetableDocumentSource 7 [6000] "https://www.highland.gov.uk/downloads/download/4/corran-ferry-timetable-and-fares" (Just "Corran Ferry")
  ]

fetchPage :: String -> IO String
fetchPage url = do
  request <- httpRequest url
  response <- timeout requestTimeoutMicros (TE.decodeUtf8With lenientDecode . getResponseBody <$> httpBS request)
  case response of
    Nothing -> error $ "Timed out fetching " <> url
    Just body -> pure $ BSC.unpack $ TE.encodeUtf8 body

fetchDocumentMetadata :: String -> String -> Application DocumentMetadata
fetchDocumentMetadata sourceLabel url = do
  result <- liftIO $ try @SomeException $ do
    request <- httpRequest url
    response <- timeout requestTimeoutMicros (httpLBS request)
    case response of
      Nothing -> error $ "Timed out fetching " <> url
      Just documentResponse -> do
        let body = getResponseBody documentResponse
            headers = getResponseHeaders documentResponse
        pure
          DocumentMetadata
            { documentMetadataContentHash = Just $ "sha256-" <> show (Crypto.hashlazy body :: Crypto.Digest Crypto.SHA256),
              documentMetadataContentType = BSC.unpack <$> lookup hContentType headers,
              documentMetadataContentLength = readMaybeString . BSC.unpack =<< lookup hContentLength headers
            }
  case result of
    Left exception -> do
      logErrorM $ "Failed to fetch timetable document metadata: " <> sourceLabel <> " - " <> url <> " - " <> show exception
      pure $ DocumentMetadata Nothing Nothing Nothing
    Right metadata -> pure metadata

httpRequest url =
  setRequestHeaders
    [ (hUserAgent, "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.36"),
      (hAccept, "text/html,application/pdf,application/octet-stream,*/*;q=0.8"),
      (hAcceptLanguage, "en-GB,en;q=0.9"),
      (hAcceptEncoding, "identity"),
      (hConnection, "keep-alive")
    ]
    <$> parseRequest url

requestTimeoutMicros :: Int
requestTimeoutMicros = 1000000 * 30

extractDocumentLinks :: String -> [DocumentLink]
extractDocumentLinks page =
  go $ parseTags page
  where
    go [] = []
    go (tag : rest)
      | isTagOpenName "a" tag =
          let (contentTags, remaining) = collectAnchor 1 [] rest
              href = fromAttrib "href" tag
              text = trim $ renderText contentTags
              dateContext = followingDateParagraphText remaining
              title =
                if null dateContext
                  then text
                  else trim text <> " (" <> titleDateContext dateContext <> ")"
           in if null href
                then go remaining
                else DocumentLink (if null title then href else title) href : go remaining
      | otherwise = go rest

    collectAnchor _ acc [] = (reverse acc, [])
    collectAnchor depth acc (tag : rest) =
      case tag of
        TagOpen "a" _ -> collectAnchor (depth + 1) (tag : acc) rest
        TagClose "a"
          | depth == 1 -> (reverse acc, rest)
          | otherwise -> collectAnchor (depth - 1) (tag : acc) rest
        _ -> collectAnchor depth (tag : acc) rest

    renderText = unwords . filter (not . null) . concatMap tagText

    tagText (TagText text) = [trim text]
    tagText _ = []

    followingDateParagraphText tags =
      case dropWhile isIgnorableAfterAnchor tags of
        TagOpen "p" _ : paragraphRest ->
          let (paragraphTags, _) = collectTag "p" 1 [] paragraphRest
              text = trim $ renderText paragraphTags
           in if looksLikeDateRange text then text else ""
        _ -> ""

    isIgnorableAfterAnchor (TagText text) = null $ trim text
    isIgnorableAfterAnchor _ = False

    collectTag _ _ acc [] = (reverse acc, [])
    collectTag tagName depth acc (tag : rest) =
      case tag of
        TagOpen name _
          | name == tagName -> collectTag tagName (depth + 1) (tag : acc) rest
        TagClose name
          | name == tagName ->
              if depth == 1
                then (reverse acc, rest)
                else collectTag tagName (depth - 1) (tag : acc) rest
        _ -> collectTag tagName depth (tag : acc) rest

    looksLikeDateRange text =
      let lowerText = lower text
       in any (`isInfixOf` lowerText) monthNames
            && any (`elem` text) ['0' .. '9']

    titleDateContext =
      replaceAll " - " " to " . trim

    monthNames =
      [ "january",
        "february",
        "march",
        "april",
        "may",
        "june",
        "july",
        "august",
        "september",
        "october",
        "november",
        "december"
      ]

normalizeDocumentLink :: String -> Maybe String -> DocumentLink -> DocumentLink
normalizeDocumentLink pageURL titlePrefix DocumentLink {..} =
  DocumentLink
    { documentLinkTitle = normalizeTitle titlePrefix documentLinkTitle documentLinkURL,
      documentLinkURL = absoluteURL pageURL documentLinkURL
    }

filterTimetableLinks :: [DocumentLink] -> [DocumentLink]
filterTimetableLinks =
  nubBy (\a b -> documentLinkURL a == documentLinkURL b)
    . filter isTimetableDocumentLink

isTimetableDocumentLink :: DocumentLink -> Bool
isTimetableDocumentLink DocumentLink {..} =
  let lowerURL = lower documentLinkURL
      lowerTitle = lower documentLinkTitle
      combined = lowerURL <> " " <> lowerTitle
      isDocumentURL =
        ".pdf" `isInfixOf` lowerURL
          || "download" `isInfixOf` lowerURL
          || "/documents/" `isInfixOf` lowerURL
      looksLikeTimetable =
        any
          (`isInfixOf` combined)
          ["timetable", "summer", "winter", "amended", "stt-", "wtt-"]
      looksLikeFareOnly =
        "fare" `isInfixOf` combined && not ("timetable" `isInfixOf` combined)
      looksLikeOfficeDocument =
        any (`isInfixOf` combined) [".doc", ".docx", " doc ", " docx "]
   in isDocumentURL && looksLikeTimetable && not looksLikeFareOnly && not looksLikeOfficeDocument

normalizeTitle :: Maybe String -> String -> String -> String
normalizeTitle titlePrefix title url =
  let fallback = fileNameTitle url
      cleanedTitle = cleanupLinkTitle title
      baseTitle =
        if null cleanedTitle || length cleanedTitle > 160 || length cleanedTitle < 3 || isGenericTimetableTitle cleanedTitle
          then fallback
          else cleanedTitle
   in case titlePrefix of
        Nothing -> baseTitle
        Just prefix
          | lower prefix `isInfixOf` lower baseTitle -> baseTitle
          | otherwise -> prefix <> ": " <> baseTitle

isGenericTimetableTitle :: String -> Bool
isGenericTimetableTitle title =
  lower (trim title) `elem` ["timetable", "imetable", "download timetable", "download"]

cleanupLinkTitle :: String -> String
cleanupLinkTitle =
  trim
    . stripTrailingPdfWord
    . stripPdfSizeText
    . replaceText "Opens in new window"
    . replaceText "opens in new window"
    . replaceText "Open in new window"
    . replaceText "open in new window"
    . replaceText "\160"
  where
    replaceText old =
      unwords . words . go
      where
        go text
          | old `isPrefixOf` text = drop (length old) text
          | otherwise =
              case text of
                [] -> []
                c : rest -> c : go rest

replaceAll :: String -> String -> String -> String
replaceAll old new text
  | null old = text
  | old `isPrefixOf` text = new <> replaceAll old new (drop (length old) text)
  | otherwise =
      case text of
        [] -> []
        c : rest -> c : replaceAll old new rest

stripPdfSizeText :: String -> String
stripPdfSizeText [] = []
stripPdfSizeText text@(c : rest)
  | c == '(' =
      let (inside, remaining) = break (== ')') rest
       in if "pdf," `isInfixOf` lower inside
            then stripPdfSizeText (drop 1 remaining)
            else c : stripPdfSizeText rest
  | otherwise = c : stripPdfSizeText rest

stripTrailingPdfWord :: String -> String
stripTrailingPdfWord title =
  let cleaned = trim title
      lowerCleaned = lower cleaned
   in if " pdf" `isSuffixOf` lowerCleaned
        then trim $ take (length cleaned - 4) cleaned
        else cleaned

fileNameTitle :: String -> String
fileNameTitle =
  unwords
    . words
    . fmap (\c -> if isAlphaNum c then c else ' ')
    . takeWhile (/= '?')
    . reverse
    . takeWhile (/= '/')
    . reverse

absoluteURL :: String -> String -> String
absoluteURL pageURL href
  | "http://" `isPrefixOf` href || "https://" `isPrefixOf` href = href
  | "//" `isPrefixOf` href = "https:" <> href
  | "/" `isPrefixOf` href = origin pageURL <> href
  | otherwise = baseDirectory pageURL <> "/" <> href

origin :: String -> String
origin url =
  case dropScheme url of
    rest -> take (length url - length rest) url <> takeWhile (/= '/') rest

baseDirectory :: String -> String
baseDirectory url =
  reverse $ dropWhile (== '/') $ dropWhile (/= '/') $ reverse url

dropScheme :: String -> String
dropScheme url
  | "https://" `isPrefixOf` url = drop 8 url
  | "http://" `isPrefixOf` url = drop 7 url
  | otherwise = url

lower :: String -> String
lower = fmap toLower

readMaybeString :: String -> Maybe Int
readMaybeString value =
  case reads value of
    [(parsed, "")] -> Just parsed
    _ -> Nothing
