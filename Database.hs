module Database
       ( dbConnection,
         login,
         logout,
         getPostById,
         getPostBySlug,
         publishPost
       ) where

import Database.HDBC
import Database.HDBC.PostgreSQL
import Post
import Data.Time.Clock

-- Database Information
host     = "localhost"
name     = "golbi"
user     = "postgres"
password = "postgresql"

connect :: IO Connection
connect = connectPostgreSQL ("host=" ++ host ++ 
                             " dbname=" ++ name ++
                             " user=" ++ user ++
                             " password=" ++ password)

dbConnection = connect

login :: IO ()
login = undefined

logout :: IO ()
logout = undefined

insertPost :: IConnection a => a -> Post -> IO ()
insertPost conn post =
    handleSql errorHandler $ do
        run conn 
            "INSERT INTO posts (title, slug, content, author, parent) VALUES (?, ?, ?, ?, ?)"
        	[toSql (postTitle post), toSql (postSlug post), 
             toSql (postContent post), toSql (postAuthor post), 
             toSql (postParent post)]
        return ()
    where errorHandler :: SqlError -> IO ()
          errorHandler e = fail ("Error inserting post.\n" ++ show e)

updatePost :: IConnection a => a -> Post -> IO ()
updatePost conn post =
    handleSql errorHandler $ do
        run conn 
            "UPDATE posts SET title=?, slug=?, content=?, author=?, parent=? WHERE id=?"
        	[toSql (postTitle post), toSql (postSlug post), 
             toSql (postContent post), toSql (postAuthor post), 
             toSql (postParent post), toSql (postId post)]
        return ()
    where errorHandler :: SqlError -> IO ()
          errorHandler e = fail ("Error updating post.\n" ++ show e)

deletePost :: IConnection a => a -> Post -> IO ()
deletePost conn post =
    handleSql errorHandler $ do
        quickQuery' conn --Promote children to post's parent
            "UPDATE posts SET parent=? WHERE parent=?"
            [toSql (postParent post), toSql (postId post)]
        run conn "DELETE FROM posts WHERE id=?" [toSql (postId post)]
        return ()
    where errorHandler :: SqlError -> IO ()
          errorHandler e = fail ("Error deleting post.\n" ++ show e)

selectPost :: IConnection a => a -> Int -> IO (Maybe [SqlValue])
selectPost conn id = do
    conn <- connect
    stmnt <- prepare conn
             "SELECT id, title, slug, content, posted, author, parent FROM posts WHERE id=?"
    execute stmnt [toSql id]
    fetchRow stmnt

selectPostBySlug :: IConnection a => a -> String -> IO (Maybe [SqlValue])
selectPostBySlug conn slug = do
    conn <- connect
    stmnt <- prepare conn
             "SELECT id, title, slug, content, posted, author, parent FROM posts WHERE slug=?"
    execute stmnt [toSql slug]
    fetchRow stmnt

selectPosts :: IConnection a => a -> Maybe (Int, Int) -> IO [[SqlValue]]
selectPosts conn Nothing =
    quickQuery' conn
        "SELECT id, title, slug, content, posted, author, parent FROM posts"
        []
selectPosts conn (Just (start, end)) =
    quickQuery' conn
        "SELECT id, title, slug, content, posted, author, parent FROM posts LIMIT ?,?"
        [toSql start, toSql end]
        
selectChildren :: IConnection a => a -> Int -> Maybe (Int, Int) -> IO [[SqlValue]]
selectChildren conn parentId Nothing =
    quickQuery conn
        "SELECT id, title, slug, content, posted, author, parent FROM posts WHERE parent=?"
        [toSql parentId]
selectChildren conn parentId (Just (start, end)) =
    quickQuery conn
        "SELECT id, title, slug, content, posted, author, parent FROM posts WHERE parent=? LIMIT ?,?"
        [toSql parentId, toSql start, toSql end]

makePost :: [Post]-> [SqlValue] -> Post
makePost children [id, title, slug, content, posted, author, parent] =
    Post { postId = fromSql id,
           postTitle = fromSql title,
           postSlug = fromSql slug,
           postContent = fromSql content,
           postTime = fromSql posted,
           postAuthor = fromSql author,
           postParent = fromSql parent, 
           postShowInNav = True, 
           postChildren = children }

getPostBySlug :: IConnection a => a -> String -> IO (Maybe Post)
getPostBySlug conn slug = do
    r <- selectPostBySlug conn slug
    return (createMaybePost [] r)
    where createMaybePost _ Nothing = Nothing
          createMaybePost y (Just x) = Just (makePost y x)

getPostById :: IConnection a => a -> Int -> IO (Maybe Post)
getPostById conn id = do
    r <- selectPost conn id
    children <- getChildren conn id Nothing
    return (createMaybePost children r)
    where createMaybePost _ Nothing = Nothing
          createMaybePost y (Just x) = Just (makePost y x)

getPosts :: Maybe (Int, Int) -> IO [Post]
getPosts limit = do
    conn <- connect
    r <- selectPosts conn limit
    return (map (makePost []) r)

getChildren :: IConnection a => a -> Int -> Maybe (Int, Int) -> IO [Post]
getChildren conn parentId limit = do
    r <- selectChildren conn parentId limit
    return (map (makePost []) r)

publishPost :: Post -> IO ()
publishPost = undefined

-- INSERT INTO posts (title, slug, content, posted, author, parent) VALUES ('Home', 'home', 'This is the content.', now(), 0, NULL);