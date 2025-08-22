module Modulus.FE.MarkdownAtomic (markdownToView, parseView) where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import Modulus.FE.MarkdownParser
import Web.Atomic.CSS
import Web.Atomic.Types.ClassName
import Web.Hyperbole

parseView :: Text -> View c ()
parseView txt = do
  case parseMarkdownText txt of
    Left _ -> text txt
    Right markdownElements -> markdownToView markdownElements

markdownToView :: [MarkdownElement] -> View c ()
markdownToView markdownElems = forM_ markdownElems $ \case
  (Header level txt) ->
    tag "p" ~ cls (ClassName ("h" <> T.pack (show level))) $ text txt
  (Paragraph inlines) -> el $ forM_ inlines solveInline
  (CodeBlock txt) -> tag "code" $ text txt
  BreakLine -> tag "br" none
  UnorderedList lst -> tag "ul" $ forM_ lst $ \txt -> tag "li" $ text txt
  OrderedList lst -> tag "ol" $ forM_ lst $ \txt -> tag "li" $ text txt
  Blockquote txt -> tag "blockquote" $ text txt

solveInline :: Inline -> View c ()
solveInline = \case
  PlainText txt -> text txt
  Bold txt -> tag "strong" $ text txt
  Italic txt -> tag "i" $ text txt
  Code txt -> tag "code" $ text txt
  Link linkTxt url -> tag "link" @ att "href" url $ text linkTxt
