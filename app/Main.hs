{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Data.Char
import           Data.List
import           Data.Ord
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time
import           Data.Yaml (decodeFileThrow)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (statusCode)
import           System.Directory

data Config = Config
  { configUsername :: ByteString
  , configToken :: ByteString
  , configIgnore :: [Text]
  , configLimit :: Int
  }

instance FromJSON Config where
  parseJSON j = do
    o <- parseJSON j
    Config <$> fmap S8.pack (o .: "username") <*> fmap S8.pack (o .: "token") <*>
      (o .: "ignore") <*> o .: "display-limit"

data Repo = Repo
  { repoPrivate :: !Bool
  , repoArchived :: !Bool
  , repoFullName :: !Text
  } deriving (Show)

instance FromJSON Repo where
  parseJSON j = do
    o <- parseJSON j
    Repo <$> (o .: "private") <*> (o .: "archived") <*>
      (o .: "full_name")

instance ToJSON Repo where
  toJSON Repo {..} =
    object
      [ "private" .= repoPrivate
      , "archived" .= repoArchived
      , "full_name" .= repoFullName
      ]

data Issue = Issue
  { issueTitle :: !Text
  , issueHtmlUrl :: !Text
  , issueUpdatedAt :: !Day
  } deriving (Show)

instance FromJSON Issue where
  parseJSON j = do
    o <- parseJSON j
    Issue <$> (o .: "title") <*> (o .: "html_url") <*>
      (do v <- o .: "updated_at"
          case parseTimeM True defaultTimeLocale "%Y-%m-%d" (take 10 v) of
            Just u -> pure u
            Nothing -> fail "Couldn't parse date.")

instance ToJSON Issue where
  toJSON Issue {..} =
    object
      [ "title" .= issueTitle
      , "html_url" .= issueHtmlUrl
      , "updated_at" .= formatTime defaultTimeLocale "%Y-%m-%d" issueUpdatedAt
      ]

main :: IO ()
main = do
  config <- decodeFileThrow "maintainer.yaml"
  createDirectoryIfMissing True "data"
  reposjson <-
    getCachedResource
      config
      "data/repos.json"
      (\perpage page ->
         "https://api.github.com/user/repos?per_page=" ++
         show perpage ++ "&page=" ++ show page)
  let repos = filter (not . isIgnoredRepo config) reposjson
  issues <-
    mapM
      (\repo ->
         fmap
           (repo, )
           (getCachedResource
              config
              ("data/issues." <>
               T.unpack
                 (T.map
                    (\c ->
                       if isAlphaNum c
                         then c
                         else '.')
                    (repoFullName repo)) <>
               ".json")
              (\perpage page ->
                 "https://api.github.com/repos/" <> T.unpack (repoFullName repo) <>
                 "/issues?state=open&sort=updated&direction=asc&per_page=" ++
                 show perpage ++ "&page=" ++ show page)))
      repos
  mapM_
    (\(_repo, is) ->
       mapM_
         (\issue ->
            T.putStrLn
              (T.unlines
                 [ issueTitle issue
                 , issueHtmlUrl issue
                 , T.pack (show (issueUpdatedAt issue))
                 ]))
         (take 1 is))
    (take
       (configLimit config)
       (sortBy
          (comparing (map issueUpdatedAt . snd))
          (filter (not . null . snd) issues)))

isIgnoredRepo ::Config -> Repo -> Bool
isIgnoredRepo config repo =
  any
    (\ignored ->
       if T.isSuffixOf "/" ignored
         then T.isPrefixOf ignored (repoFullName repo)
         else ignored == (repoFullName repo))
    (configIgnore config) ||
  repoArchived repo ||
  repoPrivate repo

getCachedResource :: (FromJSON v, ToJSON v) => Config -> FilePath -> (Int -> Int -> String) -> IO [v]
getCachedResource config cachefile mkurl = do
  exists <- doesFileExist cachefile
  if exists
    then do
      bytes <- L.readFile cachefile
      case decode bytes of
        Just as -> pure as
        Nothing -> error "Couldn't read repos.json. Delete it?"
    else do
      results <-
        downloadPaginated (configUsername config, configToken config) mkurl
      L.writeFile cachefile (encode results)
      pure results

downloadPaginated :: FromJSON v => (ByteString,ByteString) -> (Int -> Int -> String) -> IO [v]
downloadPaginated auth makeUrl = do
  manager <- newManager tlsManagerSettings
  go manager 1 []
  where
    go manager page acc = do
      request <-
        fmap (uncurry applyBasicAuth auth) (parseRequest (makeUrl perpage page))
      response <-
        httpLbs
          request
            { requestHeaders =
                requestHeaders request <> [("User-Agent", "maintainer")]
            }
          manager
      case statusCode (responseStatus response) of
        200 -> do
          case eitherDecode (responseBody response) >>= parseEither parseJSON of
            Left e -> error ("Couldn't decode JSON: " ++ e)
            Right ns -> do
              do if length ns == perpage
                   then go manager (page + 1) (ns : acc)
                   else pure (concat (ns : acc))
        _ -> error ("Bad status code: " ++ show response)
    perpage = 100
