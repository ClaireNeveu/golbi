import Database
import Template
import Post
import Network.CGI
import Text.XHtml
import System.IO
import Data.Maybe

pageHeader = header << thetitle << "Home Page"

pageBody = body << thediv << h1 << "Hello World"

pageNotFound = header << thetitle << "404 Page Not Found" +++  
               (body << h1 << "404 Page Not Found")

cgiMain template = do
    postId <- getInput "p"
    postSlug <- getInput "s"
    case (postId, postSlug) of
        (Just pId, _) -> do
            post <- liftIO (getPostById ((read pId) :: Int))
            case post of
                Nothing -> output . renderHtml $ pageNotFound
                Just p -> output . renderHtml $ elementsToHtml 
                          (expandMacros template p)
        (_, Just pSlug) -> do
            post <- liftIO (getPostBySlug pSlug)
            case post of
                Nothing -> output . renderHtml $ pageNotFound
                Just p -> output . renderHtml $ elementsToHtml 
                          (expandMacros template p)
        (_, _) -> output . renderHtml $ pageNotFound

main = do
    handle <- openFile "index.template" ReadMode
    template <- getTemplate handle
    runCGI (handleErrors (cgiMain template))
    hClose handle

elementsToHtml [] = noHtml
elementsToHtml (el:els) 
    | elType el == "text" = stringToHtml (elContent el)
    | otherwise = (tag (elType el) (elementsToHtml (elChildren el)))
                                   +++ elementsToHtml els

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
        ["getTitle"] -> [Element "text" [("content", postTitle post)] []]
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
