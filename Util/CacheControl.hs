module Util.CacheControl
   ( CacheDirective(..)
   , addCacheControl
   ) where

import Import
import qualified Text.Blaze.Internal as I
import Text.Blaze.Html.Renderer.String
import Text.Regex
import qualified Data.Text as T

data CacheDirective = Private
                    | Public
                    | NoCache
                    | NoStore
                    | NoTransform
                    | MustRevalidate
                    | ProxyRevalidate
                    | MaxAge Int
                    | SMaxAge Int

addCacheControl :: MonadHandler m => [CacheDirective] -> m ()
addCacheControl =
    addHeader "Cache-Control" . (T.intercalate ",") . (map cdString)

cdString :: CacheDirective -> T.Text
cdString Private = "private"
cdString Public = "public"
cdString NoCache = "no-cache"
cdString NoStore = "no-store"
cdString NoTransform = "no-transform"
cdString MustRevalidate = "must-revalidate"
cdString ProxyRevalidate = "proxy-revalidate"
cdString (MaxAge d) = "max-age=" `mappend` (T.pack $ show d)
cdString (SMaxAge d) = "s-maxage=" `mappend` (T.pack $ show d)
