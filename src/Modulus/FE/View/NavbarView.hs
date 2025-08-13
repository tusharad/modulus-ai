module Modulus.FE.View.NavbarView
  ( navbarView
  , NavbarView (..)
  , ToolSelectionView (..)
  ) where

import Data.Text (Text)
import Effectful (IOE)
import Modulus.Common.Types (Provider)
import Modulus.FE.Effects.StateStore
  ( StateStore (currVectorStore)
  , StateStoreEff
  , VecStore (HEB, Underarmor)
  , getState
  , modifyState
  )
import Modulus.FE.Utils
import Modulus.FE.View.ModelProviderView
import Web.Atomic.CSS
import Web.Hyperbole

data NavbarView = NavbarView Text
  deriving (Generic, ViewId)

instance HyperView NavbarView es where
  data Action NavbarView = LoadNavbarView
    deriving (Generic, ViewAction)

  type Require NavbarView = '[ModelProviders, ToolSelectionView]

  update LoadNavbarView = undefined

-- TODO: add collapse sidebar
navbarView :: Provider -> [Text] -> [Text] -> Maybe VecStore -> View NavbarView ()
navbarView p ollamaList orList mbVecStore =
  el ~ cls "top-navbar navbar navbar-expand navbar-dark" $ do
    link homeUrl ~ cls "navbar-brand d-none d-md-block" $ do
      tag "i" ~ cls "bi bi-robot" $ none
      text "Modulus AI"
    el ~ cls "ms-auto d-flex align-items-center gap-3" $ do
      hyper (ToolSelectionView 1) (setToolView mbVecStore)
      hyper (ModelProviders 1) (renderProviderListView p ollamaList orList)
      tag "dropdown" $ do
        tag "i"
          ~ cls "d-block avatar bi bi-person-circle me-2 px-2 py-2"
            @ att "data-bs-toggle" "dropdown"
            . att "aria-expanded" "false"
            . att "href" "#"
          $ none
        tag "ul" ~ cls "dropdown-menu dropdown-menu-end text-small" $ do
          tag "li"
            $ tag "a"
              ~ cls "dropdown-item"
                @ att "href" "#"
            $ tag "i" ~ cls "bi bi-gear me-2"
            $ "Settings"
          tag "li" $ tag "hr" ~ cls "dropdown-divider" $ none
          tag "li"
            $ tag "a"
              ~ cls "dropdown-item"
                @ att "href" "#"
            $ tag "i"
              ~ cls "bi bi-box-arrow-right me-2"
            $ "Logout"

data ToolSelectionView = ToolSelectionView Int
  deriving (Generic, ViewId)

instance
  ( IOE :> es
  , StateStoreEff :> es
  ) =>
  HyperView ToolSelectionView es
  where
  data Action ToolSelectionView = SetTool VecStore
    deriving (Generic, ViewAction)

  update (SetTool vecStore) = do
    st <- getState
    res <-
      if currVectorStore st == Just vecStore
        then do
          modifyState (\s -> s {currVectorStore = Nothing})
          pure Nothing
        else do
          modifyState (\s -> s {currVectorStore = Just vecStore})
          pure $ Just vecStore
    pure $ setToolView res

setToolView :: Maybe VecStore -> View ToolSelectionView ()
setToolView mbStore = do
  el ~ cls "tool-options" $ do
    button (SetTool HEB)
      ~ cls
        ( "btn tool-btn "
            <> (if mbStore == Just HEB then "active" else mempty)
        )
      $ do
        tag "i" ~ cls "bi bi-shop me-1" $ none
        text "HEB"
    button (SetTool Underarmor)
      ~ cls
        ( "btn tool-btn "
            <> (if mbStore == Just Underarmor then "active" else mempty)
        )
      $ do
        tag "i" ~ cls "bi bi-dribbble me-1" $ none
        text "Under Armour"
