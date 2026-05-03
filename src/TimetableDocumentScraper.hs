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
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isAlphaNum, toLower)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, nubBy)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time.Clock (getCurrentTime)
import qualified Data.Text.Encoding as TE
import qualified Database as DB
import Network.HTTP.Simple
  ( getResponseBody,
    getResponseHeaders,
    httpBS,
    httpLBS,
    parseRequest,
    setRequestHeaders,
  )
import Network.HTTP.Types.Header
  ( hAccept,
    hAcceptEncoding,
    hAcceptLanguage,
    hConnection,
    hContentLength,
    hContentType,
    hUserAgent,
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

fetchTimetableDocuments :: Application ()
fetchTimetableDocuments = do
  documents <- scrapeTimetableDocuments
  logInfoM $ "Saving " <> show (length documents) <> " timetable documents"
  DB.saveTimetableDocuments documents

scrapeTimetableDocuments :: Application [ScrapedTimetableDocument]
scrapeTimetableDocuments = do
  now <- liftIO getCurrentTime
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
  let documents = nubBy sameSourceURL $ concat nestedDocuments
  logInfoM $ "Found " <> show (length documents) <> " unique timetable documents"
  pure documents
  where
    sameSourceURL a b =
      scrapedTimetableDocumentSourceURL a == scrapedTimetableDocumentSourceURL b

timetableDocumentSourceLabel :: TimetableDocumentSource -> String
timetableDocumentSourceLabel TimetableDocumentSource {..} =
  fromMaybe timetableDocumentSourcePageURL timetableDocumentSourceTitlePrefix

timetableDocumentSources :: [TimetableDocumentSource]
timetableDocumentSources =
  calMacSources
    <> [ TimetableDocumentSource 2 [1000] "https://www.northlinkferries.co.uk/timetables/" (Just "NorthLink"),
         TimetableDocumentSource 3 [2000] "https://western-ferries.co.uk/timetables/" (Just "Western Ferries"),
         TimetableDocumentSource 5 [4000, 4001, 4002, 4003, 4004, 4005, 4006, 4007, 4008] "https://www.orkneyferries.co.uk/timetables" (Just "Orkney Ferries"),
         TimetableDocumentSource 4 [3000, 3001, 3002, 3003, 3004] "https://www.shetland.gov.uk/ferries/timetable" (Just "Shetland Ferries"),
         TimetableDocumentSource 7 [6000] "https://www.highland.gov.uk/downloads/download/4/corran-ferry-timetable-and-fares" (Just "Corran Ferry")
       ]

calMacSources :: [TimetableDocumentSource]
calMacSources =
  [ calMac 5 "ardrossan-brodick" "Ardrossan - Brodick",
    calMac 41 "troon-brodick" "Troon - Brodick",
    calMac 6 "claonaig-tarbert-loch-fyne-lochranza" "Claonaig/Tarbert - Lochranza",
    calMac 4 "colintraive-rhubodach" "Colintraive - Rhubodach",
    calMac 3 "wemyss-bay-rothesay" "Wemyss Bay - Rothesay",
    calMac 1 "gourock-dunoon" "Gourock - Dunoon",
    calMac 2 "tarbert-loch-fyne-portavadie" "Tarbert - Portavadie",
    calMac 7 "largs-cumbrae" "Largs - Cumbrae",
    calMac 39 "gourock-kilcreggan" "Gourock - Kilcreggan",
    calMac 36 "ardrossan-campbeltown" "Ardrossan - Campbeltown",
    calMac 9 "kennacraig-islay" "Kennacraig - Islay",
    calMac 8 "tayinloan-gigha" "Tayinloan - Gigha",
    calMac 10 "oban-colonsay-port-askaig-kennacraig" "Oban - Colonsay",
    calMac 14 "tobermory-kilchoan" "Tobermory - Kilchoan",
    calMac 13 "fionnphort-iona" "Fionnphort - Iona",
    calMac 38 "gallanach-kerrera" "Gallanach - Kerrera",
    calMac 16 "oban-coll-tiree" "Oban - Coll - Tiree",
    calMac 11 "oban-craignure" "Oban - Craignure",
    calMac 12 "lochaline-fishnish" "Lochaline - Fishnish",
    calMac 15 "oban-lismore" "Oban - Lismore",
    calMac 19 "mallaig-small-isles" "Mallaig - Small Isles",
    calMac 18 "mallaig-armadale" "Mallaig - Armadale",
    calMac 17 "sconser-raasay" "Sconser - Raasay",
    calMac 21 "ardmhor-barra-eriskay" "Ardmhor - Eriskay",
    calMac 20 "oban-castlebay" "Oban - Castlebay",
    calMac 23 "berneray-leverburgh" "Berneray - Leverburgh",
    calMac 22 "uig-lochmaddy" "Uig - Lochmaddy",
    calMac 24 "uig-tarbert-harris" "Uig - Tarbert",
    calMac 25 "stornoway-ullapool" "Stornoway - Ullapool",
    calMac 37 "mallaigoban-lochboisdale" "Mallaig/Oban - Lochboisdale"
  ]
  where
    calMac serviceID slug title =
      TimetableDocumentSource
        1
        [serviceID]
        ("https://www.calmac.co.uk/en-gb/route-information/" <> slug <> "/")
        (Just title)

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
