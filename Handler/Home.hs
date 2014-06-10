{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Forms
import Util
import Yesod.Auth
import Yesod.Paginator

getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuth
    auth <- isAdmin
    let adminStatus = authToBool auth
    let muser = fmap entityVal maid 
    (entries, pagination) <- runDB $ selectPaginated 10 [] [Desc EntryTitle]
    sess <- getSession
    (entryWidget, enctype) <- generateFormPost blankEntryForm
    defaultLayout $ do
        setTitle $ toHtml "ChrisNeveu.com"
        $(widgetFile "entries")
