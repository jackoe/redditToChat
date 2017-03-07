{-# LANGUAGE OverloadedStrings #-}
module Lib where
import Reddit
import Reddit.Types.Post
import Reddit.Types.Listing

import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

getLink :: PostContent -> Text.Text
getLink (SelfPost _ x) = x
getLink (Link x) = x
getLink TitleOnly = Text.pack "Just the title"


subRedditPosts :: RedditT IO PostListing
subRedditPosts = getPosts' (Options {pagination = Nothing, limit = Just 1}) Hot (Just . R . Text.pack $  "wallpapers")


extractPost = getLink . content

messageTest = runRedditAnon $ do
    Listing _ _ posts <- subRedditPosts
    forM_ posts $ \post -> do
        liftIO $ Text.putStrLn . tshow . extractPost $ post

tshow = Text.pack . show
