module Modulus.FE.View.NavbarView (
    navbarView 
  , NavbarView (..)
) where

import Data.Text (Text)
import Modulus.FE.Utils
import Web.Atomic.CSS
import Web.Hyperbole
import Modulus.Common.Types (Provider)
import Modulus.FE.View.ModelProviderView

data NavbarView = NavbarView Int
  deriving (Generic, ViewId)

instance HyperView NavbarView es where
  data Action NavbarView = LoadNavbarView
    deriving (Generic, ViewAction)

  type Require NavbarView = '[ModelProviders]

  update LoadNavbarView = undefined

navbarView :: Provider -> [Text] -> [Text] -> View NavbarView ()
navbarView p ollamaList orList = 
   el ~ cls "top-navbar navbar navbar-expand navbar-dark" $ do 
      -- <button class="btn btn-outline-light me-3 d-md-none" id="mobileSidebarToggle"><i class="bi bi-list"></i></button>
      link homeUrl ~ cls "navbar-brand d-none d-md-block" $ do 
          tag "i" ~ cls "bi bi-robot" $ none 
          text "Modulus AI"
      el ~ cls "ms-auto d-flex align-items-center gap-3" $ do 
        hyper (ModelProviders 1) (renderProviderListView p ollamaList orList)
        tag "dropdown" $ do 
          tag "i" ~ cls "d-block avatar bi bi-person-circle me-2 px-2 py-2" 
                  @ att "data-bs-toggle" "dropdown"
                  . att "aria-expanded" "false" 
                  . att "href" "#" $ none
          tag "ul" ~ cls "dropdown-menu dropdown-menu-end text-small" $ do
            tag "li" $ tag "a" 
                ~ cls "dropdown-item" 
                @ att "href" "#" $ tag "i" ~ cls "bi bi-gear me-2" $ "Settings"
            tag "li" $ tag "hr" ~ cls "dropdown-divider" $ none
            tag "li" $ tag "a" ~ cls "dropdown-item" 
                            @ att "href" "#" $ tag "i" 
                                ~ cls "bi bi-box-arrow-right me-2" $ "Logout"
