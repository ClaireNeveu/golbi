module Handler.Entry 
    ( getEntryR
    , postEntryR
    , postRootEntryR
    , putEntryR
    , deleteEntryR 
    ) where

import Import
import Data.Maybe (fromMaybe)
import Forms
import Data.Time (UTCTime)
import Data.Time.Format
import System.Locale
    
getEntryR :: EntryId -> Handler Html
getEntryR entryId = do
    auth <- isAdmin
    let adminStatus = authToBool auth
    entry <- runDB $ get404 entryId
    (entryWidget, enctype) <- generateFormPost $ entryForm
                              (entryAuthor entry)
                              (entryParent entry)
                              (Just (entryTitle entry))
                              (Just (entrySlug entry))
                              (Just (entryContent entry))
    defaultLayout $ do
        setTitle $ toHtml $ fromMaybe "Title" (entryTitle entry)
        let publishTime = renderTime $ entryPublishTime entry
        $(widgetFile "entry")
        
-- Creates an entry
postEntryR :: EntryId -> Handler Html
postEntryR parentId = do
    ((res, entryWidget), enctype) <- runFormPost (parentEntryForm (Just parentId))
    case res of
        FormSuccess entry -> do
            entryId <- runDB $ insert entry
            setMessage $ toHtml ("Entry created" :: Text)
            addHeader "Golbi-Redirect" "url"
            redirect $ EntryR entryId
        _ -> defaultLayout $ do
            setTitle "Invalid input"
            $(widgetFile "entryAddError")
        
-- Updates an entry
putEntryR :: EntryId -> Handler ()
putEntryR entryId = do
    ((res, _), _) <- runFormPost blankEntryForm
    case res of
        FormSuccess entry -> do
            runDB $ replace entryId entry
            setMessage $ toHtml ("Entry updated" :: Text)
            url <- toTextUrl $ EntryR entryId
            addHeader "Golbi-Redirect" url
        _ -> addHeader "Golbi-Error" "Form error"
        
-- Deletes an entry
deleteEntryR :: EntryId -> Handler Html
deleteEntryR entryId = do
    runDB $ delete entryId
    setMessage $ toHtml ("Entry deleted" :: Text)
    redirect $ HomeR
        
-- Creates an entry
postRootEntryR :: Handler Html
postRootEntryR = do
    ((res, entryWidget), enctype) <- runFormPost blankEntryForm
    case res of
        FormSuccess entry -> do
            entryId <- runDB $ insert entry
            setMessage $ toHtml ("Entry created" :: Text)
            redirect $ EntryR entryId
        _ -> defaultLayout $ do
            setTitle "Invalid input"
            $(widgetFile "entryAddError")
            
renderTime :: UTCTime -> String
renderTime = formatTime defaultTimeLocale "%Y-%m-%d at %R"