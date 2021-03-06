{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Forms
import Util
import Util.CacheControl
import Yesod.Auth
import Yesod.Paginator

getHomeR :: Handler Html
getHomeR = do
    addCacheControl [Public, MaxAge 60]
    maid <- maybeAuth
    auth <- isAdmin
    let adminStatus = authToBool auth
    let muser = fmap entityVal maid 
    (entries, pagination) <- runDB $ selectPaginated 10 [] [Desc EntryTitle]
    sess <- getSession
    (entryWidget, enctype) <- generateFormPost blankEntryForm
    defaultLayout $ do
        setTitle $ toHtml ("ChrisNeveu.com" :: String)
        $(widgetFile "entries")
