{-# LANGUAGE OverloadedStrings #-}

module Modulus.FE.MarkdownParser
  ( MarkdownElement (..)
  , parseMarkdown
  , parseMarkdownText
  , Inline (..)
  ) where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

data MarkdownElement
  = Header Int Text
  | Paragraph [Inline]
  | CodeBlock Text
  | BreakLine
  | UnorderedList [Text]
  | OrderedList [Text]
  | Blockquote Text
  deriving (Show, Eq)

data Inline
  = PlainText Text
  | Bold Text
  | Italic Text
  | Code Text
  | Link Text Text -- Link text, URL
  deriving (Show, Eq)

parseMarkdown :: Parser [MarkdownElement]
parseMarkdown = many (markdownElement <* optional eol) <* eof

markdownElement :: Parser MarkdownElement
markdownElement =
  choice
    [ header
    , codeBlock
    , blockquote
    , unorderedList
    , orderedList
    , breakLine
    , paragraph
    ]

header :: Parser MarkdownElement
header = do
  level <- length <$> some (char '#')
  space1
  content <- T.pack <$> someTill anySingle eol
  return $ Header level content

codeBlock :: Parser MarkdownElement
codeBlock = do
  void $ string "```"
  void $ optional (someTill anySingle eol)
  content <- T.pack <$> manyTill anySingle (string "```")
  return $ CodeBlock content

blockquote :: Parser MarkdownElement
blockquote = do
  void $ char '>'
  void $ optional (char ' ')
  content <- T.pack <$> someTill anySingle eol
  return $ Blockquote content

unorderedList :: Parser MarkdownElement
unorderedList = do
  items <- some listItem
  return $ UnorderedList items
  where
    listItem = do
      void $ char '-'
      space1
      T.pack <$> someTill anySingle eol

orderedList :: Parser MarkdownElement
orderedList = do
  items <- some listItem
  return $ OrderedList items
  where
    listItem = do
      void $ some digitChar
      void $ char '.'
      space1
      T.pack <$> someTill anySingle eol

paragraph :: Parser MarkdownElement
paragraph = do
  content <- some inline
  return $ Paragraph content

inline :: Parser Inline
inline =
  choice
    [ bold
    , italic
    , inlineCode
    , link
    , plainText
    ]

bold :: Parser Inline
bold = do
  void $ string "**"
  content <- T.pack <$> someTill anySingle (string "**")
  return $ Bold content

italic :: Parser Inline
italic = do
  void $ char '*'
  notFollowedBy (char '*')
  content <- T.pack <$> someTill anySingle (char '*')
  return $ Italic content

inlineCode :: Parser Inline
inlineCode = do
  void $ char '`'
  content <- T.pack <$> someTill anySingle (char '`')
  return $ Code content

link :: Parser Inline
link = do
  void $ char '['
  linkText <- T.pack <$> someTill anySingle (char ']')
  void $ char '('
  url <- T.pack <$> someTill anySingle (char ')')
  return $ Link linkText url

plainText :: Parser Inline
plainText = do
  content <- T.pack <$> some (noneOf ("\n\r*`[" :: String))
  return $ PlainText content

breakLine :: Parser MarkdownElement
breakLine = do
  void $ some newline
  return BreakLine

parseMarkdownText ::
  Text -> Either (ParseErrorBundle Text Void) [MarkdownElement]
parseMarkdownText = parse parseMarkdown ""
