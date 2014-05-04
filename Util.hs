module Util
   ( excerptHtml
   ) where

import Import
import qualified Text.Blaze.Internal as I
import Text.Blaze.Html.Renderer.String
import Text.Regex

excerptHtml :: Int -> Html -> Html
excerptHtml n html = toHtml $ excerpt $ stripTags $ renderHtml $ I.contents html
    where stripTags t = subRegex regex t ""
          regex = mkRegex "<.*?>"
          excerpt t = if (length t > n)
                      then take n t ++ "..."
                      else t