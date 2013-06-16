module Template
       ( Element(..),
         elContent,
         getAttr,
         getTemplate
       ) where  

import Text.HTML.TagSoup
import Text.StringLike (StringLike)
import Data.List
import Data.List.Split
import Data.Char
import Data.Maybe
import System.IO

data Element = Element {
    elType :: String,
    elAttrs :: [Attribute String],
    elChildren :: [Element]
  } deriving (Eq, Show)

elContent el = getAttr "content" (elAttrs el)

getAttr name [] = ""
getAttr name ((a, s):attrs) = 
    if a == name
      then s 
      else getAttr name attrs

getTemplate :: Handle -> IO [Element]
getTemplate handle = do
    tString <- hGetContents handle
    return (tagsToElements (parseTags tString))

tagsToElements :: [Tag String] -> [Element]
tagsToElements (t:ts) = 
    case t of
        (TagOpen s a) -> openTag (splitAtElem (TagClose s) ts)
        	where openTag (f, r) = [Element s a (tagsToElements f)] 
                  	++ tagsToElements (tail r)
        (TagText s) -> if all isSpace s
                         then [] ++ (tagsToElements ts)
                         else separateMacros (filter (not . isControl) s)
                              ++ (tagsToElements ts)
        _ -> [] ++ (tagsToElements ts)
tagsToElements [] = []

expandMacros :: [Element] -> [Element]
expandMacros (e:es) = undefined

splitAtElem :: Eq a => a -> [a] -> ([a], [a])
splitAtElem x ys = splitAt (fromMaybe (length ys) (elemIndex x ys)) ys

separateMacros :: String -> [Element]
separateMacros str = loop str [] [] 
    where loop [] acc pairList = 
              if null acc 
                then pairList   
                else pairList ++ [Element "text" [("content", acc)] []]
          loop (x:xs) acc pairList
              | x == '[' = 
                  if null acc
                    then loop xs "" pairList   
                    else loop xs "" (pairList ++ [Element "text" 
                                                  [("content",  acc)] 
                                                  []])
              | x == ']' = 
                  if null acc
                    then loop xs "" pairList   
                    else loop xs "" (pairList ++ [Element "macro"
                                                  [("action",  acc)] 
                                                  []])
              | otherwise = loop xs (acc++[x]) pairList

