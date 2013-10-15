
{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Conduit -- the main module
import qualified Data.ByteString.Lazy.Char8 as L

jsonData :: L.ByteString
jsonData = "payload={\"ref\":\"refs/heads/develop\",\"after\":\"df29e7a8ffd5d323eafb4cab65691c40ed868b02\",\"before\":\"844b5738fadc80437a1be63f6104575e7cf84485\",\"created\":false,\"deleted\":false,\"forced\":false,\"compare\":\"https://github.com/some/repo/compare/848b52a8fadc...d2f9e7a8ffd2\",\"commits\":[{\"id\":\"df29e2a8f226d383ea3b4cab65691c40ed868b02\",\"distinct\":true,\"message\":\"Added a readme.\",\"timestamp\":\"2013-10-02T18:43:18-07:00\",\"url\":\"https://github.com/some/repo/commit/d3f2e7a2ffd2d383eafb4cab65691c40ed868b02\",\"author\":{\"name\":\"Some Person\",\"email\":\"some.person@what.com\",\"username\":\"what\"},\"committer\":{\"name\":\"Some Name\",\"email\":\"some.commiter@what.co.jp\",\"username\":\"what-some.commiter\"},\"added\":[\"README\"],\"removed\":[],\"modified\":[]}],\"head_commit\":{\"id\":\"df29e7a8f3d6d383eafb4cab65691c40ed868b02\",\"distinct\":true,\"message\":\"Added a readme.\",\"timestamp\":\"2013-10-02T18:43:18-07:00\",\"url\":\"https://github.com/some/repo/commit/df29e7a8ffd6d383eafb4cab65691c40ed868b02\",\"author\":{\"name\":\"Some Name\",\"email\":\"some.commiter@what.co.jp\",\"username\":\"what-some.commiter\"},\"committer\":{\"name\":\"Some Name\",\"email\":\"some.commiter@what.co.jp\",\"username\":\"what-some.commiter\"},\"added\":[\"README\"],\"removed\":[],\"modified\":[]},\"repository\":{\"id\":10593550,\"name\":\"repo\",\"url\":\"https://github.com/some/repo\",\"description\":\"PHP repo\",\"watchers\":1,\"stargazers\":1,\"forks\":0,\"fork\":false,\"size\":3048,\"owner\":{\"name\":\"some\",\"email\":null},\"private\":true,\"open_issues\":0,\"has_issues\":true,\"has_downloads\":true,\"has_wiki\":true,\"language\":\"PHP\",\"created_at\":1370832166,\"pushed_at\":1380764618,\"master_branch\":\"develop\",\"organization\":\"some\"},\"pusher\":{\"name\":\"what-some.commiter\",\"email\":\"some.commiter@what.co.jp\"}}"

main :: IO ()
main = do
    req'' <- parseUrl "http://localhost/github-updater/github-updater.fcgi"

    let req' = req''
           { method = "POST"
           --, requestHeaders = [("Content-Type", "application/json")]
           , requestBody = RequestBodyLBS jsonData
           , redirectCount = 0
           --, checkStatus = \_ _ -> Nothing
           --, responseTimeout = Just 30000000
           , responseTimeout = Just 1000000
           }
    --let req = applyBasicAuth "somename" "somepass" req'
    let req = req'

    res2 <- withManager $ httpLbs req
    L.putStrLn $ responseBody res2

