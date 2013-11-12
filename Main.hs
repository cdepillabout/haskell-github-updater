{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Data.Aeson (decode, FromJSON)
import Data.ByteString.Lazy.Char8 (ByteString, unpack)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Foreign.C.Error
import GHC.Generics (Generic)
import GHC.IO.Exception
import Network.FastCGI
import System.IO
import System.IO.Error
import System.Lock.FLock
import System.Log.Logger
import System.Log.Handler.Simple (streamHandler, GenericHandler)
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import System.Posix.User
import System.Process
import Text.XHtml
import Text.Regex.PCRE ((=~))


_DEBUG_LEVEL :: Priority
--_DEBUG_LEVEL = WARNING
--_DEBUG_LEVEL = INFO
_DEBUG_LEVEL = DEBUG


data GithubResponse = GithubResponse { ref :: !Text } deriving (Show, Generic)

instance FromJSON GithubResponse

parseJsonByHand :: String -> IO String
parseJsonByHand payload = do
    let (_, _, _, groups) = (payload =~ ("\\{\"ref\":\"(.*?)\"" :: String)) :: (String, String, String, [String])
    case groups of
        [] -> return ""
        (match:_) -> return match

page :: Maybe String -> Html
page Nothing = (body << h1 << ("Thanks GitHub!!"::String)) +++ (p << ("Request: Nothing!"::String))
page (Just request) = (body << h1 << ("Thanks GitHub!!"::String)) +++ (p << (("Request: "::String) ++ request))

getRepo :: ByteString -> Maybe Text
getRepo jsonPayload = do
    jsonResponse <- decode jsonPayload
    return $ ref jsonResponse

repoPath :: String -> Maybe FilePath
repoPath "refs/heads/develop" = Just "/var/www/html/api/latest"
repoPath "refs/heads/v0_hotfix" = Just "/var/www/html/api/0"
repoPath "refs/heads/v3_hotfix" = Just "/var/www/html/api/3"
repoPath "refs/heads/v4_hotfix" = Just "/var/www/html/api/4"
repoPath "refs/heads/v5_hotfix" = Just "/var/www/html/api/5"
repoPath _ = Nothing

repoRemoteBranch :: String -> Maybe String
repoRemoteBranch "refs/heads/develop" = Just "origin/develop"
repoRemoteBranch "refs/heads/v0_hotfix" = Just "origin/v0_hotfix"
repoRemoteBranch "refs/heads/v3_hotfix" = Just "origin/v3_hotfix"
repoRemoteBranch "refs/heads/v4_hotfix" = Just "origin/v4_hotfix"
repoRemoteBranch "refs/heads/v5_hotfix" = Just "origin/v5_hotfix"
repoRemoteBranch _ = Nothing

repoLocalBranch :: String -> Maybe String
repoLocalBranch "refs/heads/develop" = Just "develop"
repoLocalBranch "refs/heads/v0_hotfix" = Just "v0_hotfix"
repoLocalBranch "refs/heads/v3_hotfix" = Just "v3_hotfix"
repoLocalBranch "refs/heads/v4_hotfix" = Just "v4_hotfix"
repoLocalBranch "refs/heads/v5_hotfix" = Just "v5_hotfix"
repoLocalBranch _ = Nothing

silence :: String
silence = if _DEBUG_LEVEL == DEBUG then " " else " >/dev/null "

gitCommand :: String -> String -> String -> String
gitCommand path remoteBranch localBranch =
    (if _DEBUG_LEVEL == DEBUG then "set -x && " else "") ++
    "cd " ++ path ++ silence ++ " && " ++
    "git stash " ++ silence ++ " && " ++
    "git fetch -p origin " ++ silence ++ " && " ++
    "(if [ \"$(git branch | cut -f 2 -d ' ')\" != \""++ localBranch ++ "\" ] ; then git checkout " ++ localBranch ++ silence ++ " ;  fi) && " ++
    "git merge --ff-only " ++ remoteBranch ++ silence ++ " && " ++
    "git stash pop" ++ silence

runGitCommand :: String -> String -> String -> String -> IO ()
runGitCommand mylog path remoteBranch localBranch = do
    let command = gitCommand path remoteBranch localBranch
    liftIO $ debugM mylog $ "Running command: " ++ command
    let shellProc = CreateProcess {
                cmdspec = ShellCommand command,
                cwd = Nothing,
                env = Nothing,
                std_in = Inherit,
                std_out = UseHandle stdout,
                std_err = Inherit,
                close_fds = False,
                create_group = False }
    (_, _, _, procHandle) <- createProcess shellProc
    _ <- waitForProcess procHandle
    return ()

updateRepo :: String -> String -> IO ()
updateRepo mylog repo = do
    let path = repoPath repo
    let remoteBranch = repoRemoteBranch repo
    let localBranch = repoLocalBranch repo
    case (path, remoteBranch, localBranch) of
        (Just path', Just remoteBranch', Just localBranch') -> do
            -- lock on the directory's index.php file
            catchIOError (runGitWithLock path' remoteBranch' localBranch') handler
        _ -> return ()
    where
        runGitWithLock :: String -> String -> String -> IO()
        runGitWithLock path remoteBranch localBranch = do
            withLock (path ++  "/index.php") Exclusive NoBlock $
                runGitCommand mylog path remoteBranch localBranch

        handler :: IOError -> IO ()
        handler IOError{ ioe_errno = Just errorNumber } | (Errno errorNumber) == eWOULDBLOCK =
            infoM mylog $ "Tried to update repo (" ++ repo ++ "), but another program has a lock on it."
        handler ex = warningM mylog $ "When updating repo (" ++ repo ++ "), unhandled exception: " ++ show ex

cgiMain :: CGI CGIResult
cgiMain = do
    liftIO $ hSetEncoding stderr utf8
    myStreamHandler <- fmap withFormatter $ liftIO $ streamHandler stderr _DEBUG_LEVEL
    let mylog = rootLoggerName
    liftIO $ updateGlobalLogger mylog (setLevel _DEBUG_LEVEL)
    liftIO $ updateGlobalLogger mylog (setHandlers [myStreamHandler])
    uid <- liftIO getRealUserID
    euid <- liftIO getEffectiveUserID
    liftIO $ debugM mylog $ "Real User ID: " ++ (show uid) ++ ", Effective User ID: " ++ (show euid)

    payloadString <- getInput "payload"
    payloadByteString <- getInputFPS "payload"

    liftIO $ debugM mylog $ "Payload String: " ++ (fromMaybe "(No payload uploaded!)" payloadString)

    case payloadByteString of
        Nothing -> output $ renderHtml $ page Nothing
        Just payload' -> do
            let repo = fmap encodeUtf8 $ getRepo payload'
            case repo of
                Nothing -> do
                    repo' <- liftIO $ parseJsonByHand $ unpack payload'
                    case repo' of
                        "" -> liftIO $ debugM mylog $ "Couldn't get repo to update!"
                        _ -> liftIO $ updateRepo mylog repo'
                Just repo' -> do
                    liftIO $ debugM mylog $ "Updating repo: " ++ (unpack repo')
                    liftIO $ updateRepo mylog (unpack repo')
            output $ renderHtml $ page payloadString

main :: IO ()
main = do
    runFastCGI $ handleErrors cgiMain

withFormatter :: GenericHandler Handle -> GenericHandler Handle
withFormatter handler = setFormatter handler formatter
    where formatter = simpleLogFormatter "[$time GitHub-Updater $prio] $msg"

