import Database
import Database.HDBC (disconnect)
import Template
import Post
import Network.CGI
import Text.XHtml
import System.IO
import Data.Maybe
import Control.Monad (liftM2)

--Begin Global Settings

homePageID = 1
siteName = "Chris Neveu :: "

--End Global Settings

pageHeader = header << thetitle << "Home Page"

pageBody = body << thediv << h1 << "Hello World"

pageNotFound = header << thetitle << "404 Page Not Found" +++  
               (body << h1 << "404 Page Not Found")

cgiMain handle template = do
    postId <- getInput "p"
    postSlug <- getInput "s"
    case (postId, postSlug) of
        (Just pId, _) -> do
            post <- liftIO (getPostById handle ((read pId) :: Int))
            case post of
                Nothing -> output . renderHtml $ pageNotFound
                Just p -> output . renderHtml $ elementsToHtml 
                          (expandMacros template p)
        (_, Just pSlug) -> do
            post <- liftIO (getPostBySlug handle pSlug)
            case post of
                Nothing -> output . renderHtml $ pageNotFound
                Just p -> output . renderHtml $ elementsToHtml 
                          (expandMacros template p)
        (_, _) -> do
            post <- liftIO (getPostById handle homePageID)
            case post of
                Nothing -> output . renderHtml $ pageNotFound
                Just p -> output . renderHtml $ elementsToHtml 
                          (expandMacros template p)

main = do
    handle <- openFile "index.template" ReadMode
    tString <- hGetContents handle
    dbConn <- dbConnection
    let template = parseTemplate tString
    runCGI (handleErrors (cgiMain dbConn template))
    hClose handle
    disconnect dbConn

elementsToHtml [] = noHtml
elementsToHtml (el:els) 
    | elType el == "text" = stringToHtml (elContent el)
    | otherwise = (tag (elType el) (elementsToHtml (elChildren el)))
                  ! attrToHtml (elAttrs el)
                  +++ elementsToHtml els

attrToHtml [] = []
attrToHtml ((a, s):as) = strAttr a s : attrToHtml as

expandMacros :: [Element] -> Post -> [Element]
expandMacros [] _ = []
expandMacros (el:els) post
    | elType el == "macro" = expandMacro el post ++ (expandMacros els post)
    | otherwise = (Element (elType el)
                   (elAttrs el)
                   (expandMacros (elChildren el) post))
                  : (expandMacros els post)

expandMacro :: Element -> Post -> [Element]
expandMacro el post =
    case (words (getAttr "action" (elAttrs el))) of
        ["getTitle"] -> 
            [Element "text" [("content", postTitle post)] []]
        ["getContent"] -> 
            parseTemplate (fromMaybe "" (postContent post))
        ["getChildren"] -> 
            [Element "text" 
             [("content", postTitle (head $ postChildren post))] []]
        ["getChildren", postId] -> undefined
        _ -> [el]

{-

main
-----------------------
runCGI (handleErrors cgiMain)


cgiMain
-----------------------
if (get "postSlug")
	x = displayPost (getPostBySlug (get "postSlug"))
else if  (get "postId")
	x = displayPost (getPostById (get "postId"))
else
	if (Home Page is set)
		x = displayPost Home Page
	else
		x = Default Page
output renderHtml $ pageHeader "Golbi" +++ pageBody x


displayPost p
-----------------------
thediv ! [theclass "post"] << (h1 << (postTitle p) +++ 
(paragraph << postContent p)


pageHeader t
-----------------------
header << thetitle << t
-}
