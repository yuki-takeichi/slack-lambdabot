{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Codec.Binary.UTF8.String (decodeString)
import Control.Lens (use)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, dropWhile, pack, unpack, stripPrefix)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import HTMLEntities.Decoder (htmlEncodedText)
import Lambdabot.Main
import Modules (modulesInfo)
import Prelude hiding (dropWhile, filter)
import System.Environment (lookupEnv)
import System.IO.Silently (capture)
import Web.Slack (Event(Message), Slack, SlackBot, SlackConfig(..), getId,
  runBot, selfUserId, session, slackSelf)
import Web.Slack.Message (sendMessage)

-------------------------------------------------------------------------------
-- Lambdabot
-------------------------------------------------------------------------------

lambdabot :: String -> IO String
lambdabot command = do
  let request = void $ lambdabotMain modulesInfo
        [onStartupCmds :=> [command]]
  (response, _) <- capture request
  return response

-------------------------------------------------------------------------------
-- Slack
-------------------------------------------------------------------------------

-- | Construct a @SlackConfig@, taking the Slack API token from an environment
-- variable.
envMkSlackConfig :: String -> IO SlackConfig
envMkSlackConfig key
  =  mkSlackConfig
 <$> fromMaybe (error $ key <> " not set")
 <$> lookupEnv key

-- | Construct a @SlackConfig@ from a Slack API token.
mkSlackConfig :: String -> SlackConfig
mkSlackConfig apiToken = SlackConfig { _slackApiToken = apiToken }

-- | Get a message if it is for \"me\".
getMessageForMe :: Text -> Slack a (Maybe Text)
getMessageForMe message = do
  myId <- use $ session . slackSelf . selfUserId . getId
  let atMyId = "<@" <> myId <> ">"
  return $  dropWhile (\c -> c == ':' || c == ' ')
        <$> stripPrefix atMyId message

-- | Construct a @SlackBot@ from a name. This bot will pass messages addressed
-- to it to 'lambdabot' and relay 'lambdabot''s response.
slackBot :: SlackBot a
slackBot (Message cid _ someMessage _ _ _) = do
  messageForMe <- getMessageForMe someMessage
  -- let shouldReportParseError = isJust messageForMe
  let message = fromMaybe someMessage messageForMe
  let command = decodeHtml message
  rawResponse <- liftIO (pack . decodeString <$> lambdabot (unpack command))
  let response = "```\n" <> rawResponse <> "```"
  sendMessage cid response
slackBot _ = return ()

decodeHtml :: Text -> Text
decodeHtml = toStrict . toLazyText . htmlEncodedText

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  slackConfig <- envMkSlackConfig "SLACK_API_TOKEN"
  runBot slackConfig slackBot ()
