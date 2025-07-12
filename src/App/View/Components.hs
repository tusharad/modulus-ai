{-# LANGUAGE OverloadedStrings #-}
module App.View.Components (sidebarView) where

import Web.Hyperbole
import Web.Atomic.CSS
import App.Common.Utils

sidebarView :: View c ()
sidebarView = do
  tag "button" ~ cls "btn btn-primary"
         @ att "type" "button"
         . att "data-bs-toggle" "offcanvas"
         . att "data-bs-target" "#offcanvasExample"
         . att "aria-controls" "offcanvasExample" $
    "Button with data-bs-target"

  -- Offcanvas Panel
  el ~ cls "offcanvas offcanvas-start"
     @ att "tabindex" "-1"
     . att "id" "offcanvasExample"
     . att "aria-labelledby" "offcanvasExampleLabel" $ do

    -- Offcanvas Header
    el ~ cls "offcanvas-header" $ do
      tag "h5" ~ cls "offcanvas-title" @ att "id" "offcanvasExampleLabel" $ "Offcanvas"
      tag "button" ~ cls "btn-close"
             @ att "type" "button"
             . att "data-bs-dismiss" "offcanvas"
             . att "aria-label" "Close" $ none

    -- Offcanvas Body
    el ~ cls "offcanvas-body" $ do
      el ~ fontSize 16 $ "Some text as placeholder. In real life you can have the elements you have chosen. Like, text, images, lists, etc."

      -- Dropdown inside offcanvas
      el ~ cls "dropdown mt-3" $ do
        tag "button" ~ cls "btn btn-secondary dropdown-toggle"
               @ att "type" "button"
               . att "data-bs-toggle" "dropdown" $
          "Dropdown button"

        ul ~ cls "dropdown-menu" $ do
          li $ link homeUrl ~ cls "dropdown-item" $ "Action"
          li $ link homeUrl ~ cls "dropdown-item" $ "Another action"
          li $ link homeUrl ~ cls "dropdown-item" $ "Something else here"
