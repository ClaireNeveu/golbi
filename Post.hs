module Post
       ( Post(..),
         DBPost
       ) where

import Data.Time.Clock

data Post = Post {
    postId :: Int,
    postTitle :: String,
    postSlug :: String,
    postContent :: Maybe String,
    postTime :: UTCTime,
    postAuthor :: Int,
    postParent :: Maybe Int,
    postShowInNav :: Bool
    } deriving ()

type DBPost = Int
	-> String 
    -> String
    -> String
    -> UTCTime
    -> Int
    -> Int
    -> Bool