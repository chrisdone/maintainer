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
import qualified Data.HashMap.Strict as HM
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
  let repos = mapMaybe (getWantedRepo config) reposjson
  issues <-
    fmap
      (sortBy (comparing (\(_name, _title, _url, String updated) -> updated)) .
       take (configLimit config) . concat)
      (mapM
         (\fullName -> do
            issues <-
              getCachedResource
                config
                ("data/issues." <>
                 T.unpack
                   (T.map
                      (\c ->
                         if isAlphaNum c
                           then c
                           else '.')
                      fullName) <>
                 ".json")
                (\perpage page ->
                   "https://api.github.com/repos/" <> T.unpack fullName <>
                   "/issues?state=open&sort=updated&direction=asc&per_page=" ++
                   show perpage ++ "&page=" ++ show page)
            pure
              (mapMaybe
                 (\issue ->
                    (fullName, , , ) <$> HM.lookup "title" issue <*>
                    HM.lookup "html_url" issue <*>
                    HM.lookup "updated_at" issue)
                 issues))
         repos)
  mapM_
    (\(_name, String title, String url, String updated) ->
       T.putStrLn (T.unlines [title, url, updated]))
    issues

getWantedRepo ::Config -> HM.HashMap Text Value -> Maybe Text
getWantedRepo config result =
  case HM.lookup "full_name" result of
    Just (String fullName) ->
      if any
           (\ignored ->
              if T.isSuffixOf "/" ignored
                then T.isPrefixOf ignored fullName
                else ignored == fullName)
           (configIgnore config) ||
         HM.lookup "archived" result == Just (Bool True) ||
         HM.lookup "private" result == Just (Bool True)
        then Nothing
        else Just fullName
    _ -> Nothing

getCachedResource :: Config -> FilePath -> (Int -> Int -> String) -> IO [Object]
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

downloadPaginated :: (ByteString,ByteString) -> (Int -> Int -> String) -> IO [Object]
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
