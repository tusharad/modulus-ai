module App.DB
  ( ChatMessage (..)
  , Conversation (..)
  , ConversationWithCount (..)
  , CRole (..)
  , createTables
  , withDatabase
  , initializeDatabase
  , getConversation
  , createConversation
  , getAllConversations
  , deleteConversation
  , addMessage
  , updateMessage
  , deleteMessage
  , deleteConversationMessages
  , updateConversationTitle
  , getMessage
  , getConversationMessages
  , getRecentMessages
  , getMessageCount
  , searchMessages
  , getConversationsWithMessageCount
  ) where

import Data.Text (Text)
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Control.Exception (bracket)
import Data.Char
import Database.SQLite.Simple.Ok (Ok (..))
import GHC.Generics

data CRole = User | Assistant | System
  deriving (Show, Read, Eq, Generic)

instance ToField CRole where
  toField x = toField $ map toLower (show x)

instance FromField CRole where
  fromField f = do
    case (fromField f :: Ok Text) of
      (Ok "user") -> pure User
      (Ok "assistant") -> pure Assistant
      _ -> pure System

data Conversation = Conversation
  { convId :: Int
  , convTitle :: Text
  , convCreatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance FromRow Conversation where
  fromRow = Conversation <$> field <*> field <*> field

instance ToRow Conversation where
  toRow (Conversation cId title createdAt) = toRow (cId, title, createdAt)

data ChatMessage = ChatMessage
  { msgId :: Int
  , msgRole :: CRole
  , msgContent :: Text
  , msgCreatedAt :: UTCTime
  , msgConversationId :: Int
  }
  deriving (Show, Generic)

instance FromRow ChatMessage where
  fromRow = ChatMessage <$> field <*> field <*> field <*> field <*> field

instance ToRow ChatMessage where
  toRow (ChatMessage mId role1 content1 createdAt convId1) =
    toRow (mId, role1, content1, createdAt, convId1)

-- Database Schema
createTables :: Connection -> IO ()
createTables conn = do
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS conversations (\
    \id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \title TEXT NOT NULL,\
    \created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP\
    \)"

  execute_
    conn
    "CREATE TABLE IF NOT EXISTS chat_history (\
    \message_id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \crole TEXT NOT NULL,\
    \message_content TEXT NOT NULL,\
    \created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,\
    \conversation_id INTEGER NOT NULL,\
    \attachment TEXT,\
    \FOREIGN KEY (conversation_id) REFERENCES conversations(id) ON DELETE CASCADE\
    \)"

  execute_
    conn
    "CREATE INDEX IF NOT EXISTS idx_chat_history_conversation_id \
    \ON chat_history(conversation_id)"

  execute_
    conn
    "CREATE INDEX IF NOT EXISTS idx_chat_history_created_at \
    \ON chat_history(created_at)"

-- Database Operations
withDatabase :: String -> (Connection -> IO a) -> IO a
withDatabase dbPath = bracket (open dbPath) close

initializeDatabase :: String -> IO ()
initializeDatabase dbPath = withDatabase dbPath createTables

-- Conversation Operations
createConversation :: Connection -> Text -> IO Int
createConversation conn title = do
  execute conn "INSERT INTO conversations (title) VALUES (?)" (Only title)
  fromIntegral <$> lastInsertRowId conn

getConversation :: Connection -> Int -> IO (Maybe Conversation)
getConversation conn convId1 = do
  rows <-
    query
      conn
      "SELECT id, title, created_at FROM conversations WHERE id = ?"
      (Only convId1)
  case rows of
    [conv] -> return $ Just conv
    _ -> return Nothing

getAllConversations :: Connection -> IO [Conversation]
getAllConversations conn =
  query_ conn "SELECT id, title, created_at FROM conversations ORDER BY created_at DESC"

updateConversationTitle :: Connection -> Int -> Text -> IO ()
updateConversationTitle conn convId1 newTitle =
  execute conn "UPDATE conversations SET title = ? WHERE id = ?" (newTitle, convId1)

deleteConversation :: Connection -> Int -> IO ()
deleteConversation conn convId1 =
  execute conn "DELETE FROM conversations WHERE id = ?" (Only convId1)

-- Chat Message Operations
addMessage :: Connection -> CRole -> Text -> Int -> Maybe Text -> IO Int
addMessage conn role content conversationId mbFilePath = do
  now <- getCurrentTime
  execute
    conn
    ( "INSERT INTO chat_history (crole, message_content, created_at, conversation_id, attachment)"
        <> "VALUES (?, ?, ?, ?, ?)"
    )
    (role, content, now, conversationId, mbFilePath)
  fromIntegral <$> lastInsertRowId conn

getMessage :: Connection -> Int -> IO (Maybe ChatMessage)
getMessage conn messageId = do
  rows <-
    query
      conn
      ( "SELECT message_id, crole, message_content, created_at, conversation_id"
          <> "FROM chat_history WHERE message_id = ?"
      )
      (Only messageId)
  case rows of
    [msg] -> return $ Just msg
    _ -> return Nothing

getConversationMessages :: Connection -> Int -> IO [ChatMessage]
getConversationMessages conn conversationId =
  query
    conn
    "SELECT message_id, crole, message_content, created_at, conversation_id \
    \FROM chat_history WHERE conversation_id = ? ORDER BY created_at ASC"
    (Only conversationId)

getRecentMessages :: Connection -> Int -> Int -> IO [ChatMessage]
getRecentMessages conn conversationId limit =
  query
    conn
    "SELECT message_id, crole, message_content, created_at, conversation_id \
    \FROM chat_history WHERE conversation_id = ? ORDER BY created_at DESC LIMIT ?"
    (conversationId, limit)

updateMessage :: Connection -> Int -> Text -> IO ()
updateMessage conn messageId newContent =
  execute
    conn
    "UPDATE chat_history SET message_content = ? WHERE message_id = ?"
    (newContent, messageId)

deleteMessage :: Connection -> Int -> IO ()
deleteMessage conn messageId =
  execute conn "DELETE FROM chat_history WHERE message_id = ?" (Only messageId)

deleteConversationMessages :: Connection -> Int -> IO ()
deleteConversationMessages conn conversationId =
  execute conn "DELETE FROM chat_history WHERE conversation_id = ?" (Only conversationId)

-- Utility Functions
getMessageCount :: Connection -> Int -> IO Int
getMessageCount conn conversationId = do
  [Only count] <-
    query
      conn
      "SELECT COUNT(*) FROM chat_history WHERE conversation_id = ?"
      (Only conversationId)
  return count

searchMessages :: Connection -> Text -> IO [ChatMessage]
searchMessages conn searchTerm =
  query
    conn
    "SELECT message_id, crole, message_content, created_at, conversation_id \
    \FROM chat_history WHERE message_content LIKE ? ORDER BY created_at DESC"
    (Only $ "%" <> searchTerm <> "%")

-- Conversation with message count
data ConversationWithCount = ConversationWithCount
  { cwcConversation :: Conversation
  , cwcMessageCount :: Int
  }
  deriving (Show)

getConversationsWithMessageCount :: Connection -> IO [ConversationWithCount]
getConversationsWithMessageCount conn = do
  rows <-
    query_
      conn
      "SELECT c.id, c.title, c.created_at, COUNT(ch.message_id) as message_count \
      \FROM conversations c \
      \LEFT JOIN chat_history ch ON c.id = ch.conversation_id \
      \GROUP BY c.id, c.title, c.created_at \
      \ORDER BY c.created_at DESC"
  return $
    map
      ( \(cId, title, createdAt, count) ->
          ConversationWithCount (Conversation cId title createdAt) count
      )
      rows
