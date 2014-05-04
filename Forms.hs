module Forms
   ( entryForm
   , blankEntryForm
   , parentEntryForm
   ) where

import Import
import Data.Time (getCurrentTime)
import Yesod.Form.Nic (nicHtmlField)

entryForm :: Maybe UserId -> Maybe EntryId -> Maybe (Maybe Text) -> Maybe (Maybe Text) -> Maybe Html -> Form Entry
entryForm authorId parentId title slug content = renderDivs $ Entry
    <$> pure authorId
    <*> pure parentId
    <*> lift (liftIO getCurrentTime)
    <*> aopt textField "Title" title
    <*> aopt textField "Slug" slug
    <*> areq nicHtmlField "Content" content

blankEntryForm :: Form Entry
blankEntryForm = entryForm Nothing Nothing Nothing Nothing Nothing

parentEntryForm :: Maybe EntryId -> Form Entry
parentEntryForm pid = entryForm Nothing pid Nothing Nothing Nothing