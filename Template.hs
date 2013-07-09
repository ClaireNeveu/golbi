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
tagsToElements [] = []
tagsToElements ((TagOpen s a):ts) = 
    Element s a (tagsToElements chil) : (tagsToElements els) 
        where (chil, els) = tagChildren ts [] 0
              tagChildren (t@(TagClose _):ts) acc 0 = (acc, ts)
              tagChildren (t@(TagClose _):ts) acc inc = 
                  tagChildren ts (acc ++ [t]) (inc - 1)
              tagChildren (t@(TagOpen _ _):ts) acc inc = 
                  tagChildren ts (acc ++ [t]) (inc + 1)
              tagChildren (t:ts) acc inc = 
                  tagChildren ts (acc ++ [t]) inc
tagsToElements ((TagText s):ts) = 
    if all isSpace s
       then tagsToElements ts
       else separateMacros (filter (not . isControl) s) ++ (tagsToElements ts)
tagsToElements (_:ts) = tagsToElements ts

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

