{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Data.Char
import           Data.List
import           Data.Ord
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Data.Time
import           Data.Yaml (decodeFileThrow)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (statusCode)
import           System.Directory
import           System.Environment
import           Text.Printf

data Config = Config
  { configUsername :: ByteString
  , configToken :: ByteString
  , configIgnore :: [Text]
  , configLimit :: Int
  , configHideIssueless :: Bool
  , configStalenessDays :: Integer
  , configAssigneeCheck :: Bool
  }

instance FromJSON Config where
  parseJSON j = do
    o <- parseJSON j
    Config <$> fmap S8.pack (o .: "username") <*> fmap S8.pack (o .: "token") <*>
      (o .: "ignore") <*>
      o .: "display-limit" <*>
      o .: "hide-issueless" <*>
      o .: "staleness-days" <*>
      o .: "assignee-check"

data Repo = Repo
  { repoPrivate :: !Bool
  , repoArchived :: !Bool
  , repoFullName :: !Text
  , repoOpenIssuesCount :: Int
  } deriving (Show)

instance FromJSON Repo where
  parseJSON j = do
    o <- parseJSON j
    Repo <$> (o .: "private") <*> (o .: "archived") <*>
      (o .: "full_name") <*> (o .: "open_issues_count")

instance ToJSON Repo where
  toJSON Repo {..} =
    object
      [ "private" .= repoPrivate
      , "archived" .= repoArchived
      , "full_name" .= repoFullName
      , "open_issues_count" .= repoOpenIssuesCount
      ]

data Issue = Issue
  { issueTitle :: !Text
  , issueHtmlUrl :: !Text
  , issueUpdatedAt :: !Day
  , issueAssignees :: [Text]
  } deriving (Show)

instance FromJSON Issue where
  parseJSON j = do
    o <- parseJSON j
    Issue <$> (o .: "title") <*> (o .: "html_url") <*>
      (do v <- o .: "updated_at"
          case parseTimeM True defaultTimeLocale "%Y-%m-%d" (take 10 v) of
            Just u -> pure u
            Nothing -> fail "Couldn't parse date.") <*> (o .: "assignees" >>= mapM (.: "login"))

instance ToJSON Issue where
  toJSON Issue {..} =
    object
      [ "title" .= issueTitle
      , "html_url" .= issueHtmlUrl
      , "updated_at" .= formatTime defaultTimeLocale "%Y-%m-%d" issueUpdatedAt
      , "assignees" .= map (\a -> object ["login" .= a]) issueAssignees
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
              (repoIssuesFilename repo)
              (\perpage page ->
                 "https://api.github.com/repos/" <> T.unpack (repoFullName repo) <>
                 "/issues?state=open&sort=updated&direction=asc&per_page=" ++
                 show perpage ++ "&page=" ++ show page)))
      repos
  args <- getArgs
  now <- fmap utctDay getCurrentTime
  case args of
    ("refresh":projects) -> do
      mapM_
        (\(repo, _) -> do
           putStrLn ("Removing " ++ repoIssuesFilename repo)
           removeFile (repoIssuesFilename repo))
        (if null projects
           then (dashboardIssues config issues)
           else filter
                  (\(repo, _) -> elem (repoFullName repo) (map T.pack projects))
                  (dashboardIssues config issues))
    ["health"] ->
      putStrLn
        ("Repositories with open issues assigned to you or unassigned\n\n" ++
         tablize
           ([ (True, "Repo")
            , (True, "Staleness")
            , (True, "Issues")
            , (True, "Stale Issues")
            , (True, "Avg. last update")
            ] :
            map
              (\(repo, is) ->
                 let staleIssues =
                       (filter
                          ((> configStalenessDays config) .
                           diffDays now . issueUpdatedAt)
                          is)
                  in [ (True, T.unpack (repoFullName repo))
                     , ( True
                       , printf
                           "%3.2f%%"
                           (100 *
                            (fromIntegral (length staleIssues) /
                             fromIntegral (length is)) :: Float))
                     , (True, show (length is))
                     , (True, show (length staleIssues))
                     , ( True
                       , printf
                           "%.2f days"
                           (sum
                              (map
                                 (fromIntegral . diffDays now . issueUpdatedAt)
                                 is) /
                            fromIntegral (length is) :: Float))
                     ])
              (sortBy
                 (flip (comparing (length . snd)))
                 (filter
                    (\(_, rissues) ->
                       all
                         (noAssigneeOrMe config)
                         rissues)
                    (filter
                       (if configHideIssueless config
                          then not . null . snd
                          else const True)
                       issues)))))
    _ ->
      mapM_
        (\(_repo, is) ->
           mapM_
             (\issue ->
                T.putStrLn
                  (T.unlines
                     [ issueTitle issue
                     , issueHtmlUrl issue
                     , T.pack (show (issueUpdatedAt issue)) <>
                       (if diffDays now (issueUpdatedAt issue) >
                           configStalenessDays config
                          then " (STALE)"
                          else " (alive)")
                     ]))
             (take 1 is))
        (take (configLimit config) (dashboardIssues config issues))
  where
    dashboardIssues _config =
      sortBy (comparing (map issueUpdatedAt . snd)) .
      map (second (take 1 . sortBy (comparing issueUpdatedAt))) .
      filter (not . null . snd)

noAssigneeOrMe :: Config -> Issue -> Bool
noAssigneeOrMe config issue =
  if configAssigneeCheck config
     then null (issueAssignees issue) || elem (T.decodeUtf8 (configUsername config)) (issueAssignees issue)
     else True

repoIssuesFilename :: Repo -> String
repoIssuesFilename repo =
  "data/issues." <>
  T.unpack
    (T.map
       (\c ->
          if isAlphaNum c
            then c
            else '.')
       (repoFullName repo)) <>
  ".json"

-- | Make a table out of a list of rows.
tablize :: [[(Bool,String)]] -> String
tablize xs =
  intercalate "\n" (map (intercalate "  " . map fill . zip [0 ..]) xs)
  where
    fill (x', (left, text')) =
      printf ("%" ++ direction ++ show width ++ "s") text'
      where
        direction =
          if left
            then "-"
            else ""
        width = maximum (map (length . snd . (!! x')) xs)

isIgnoredRepo ::Config -> Repo -> Bool
isIgnoredRepo config repo =
  any
    (\ignored ->
       if T.isSuffixOf "/" ignored
         then T.isPrefixOf ignored (repoFullName repo)
         else ignored == (repoFullName repo))
    (configIgnore config) ||
  repoArchived repo ||
  repoPrivate repo ||
  repoOpenIssuesCount repo == 0

getCachedResource :: (FromJSON v, ToJSON v) => Config -> FilePath -> (Int -> Int -> String) -> IO [v]
getCachedResource config cachefile mkurl = do
  exists <- doesFileExist cachefile
  if exists
    then do
      bytes <- L.readFile cachefile
      case eitherDecode bytes of
        Right as -> pure as
        Left e -> error ("Couldn't read " ++ cachefile ++ e)
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
      putStrLn ("Downloading " ++ makeUrl perpage page)
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
