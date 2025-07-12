{-# LANGUAGE OverloadedStrings #-}

module App.View.Header (headerView) where

import Web.Hyperbole

import App.Common.Utils
import Web.Atomic.CSS

colorModeToggle :: View c ()
colorModeToggle =
    tag "button"
        ~ cls "btn"
            @ att "onClick" "toggleColorMode()"
        $ text "Toggle Color Mode"

-- | A simple static navbar view
headerView :: View c ()
headerView = el ~ cls "navbar navbar-expand-lg bg-body-tertiary" $ do
    el ~ cls "container-fluid" $ do
        -- Brand
        link homeUrl ~ cls "navbar-brand" $ text "Navbar"

        -- Toggler Button
        tag "button"
            ~ cls "navbar-toggler"
                @ att "type" "button"
                . att "data-bs-toggle" "collapse"
                . att "data-bs-target" "#navbarSupportedContent"
                . att "aria-controls" "navbarSupportedContent"
                . att "aria-expanded" "false"
                . att "aria-label" "Toggle navigation"
            $ tag "span" ~ cls "navbar-toggler-icon" $ none

        -- Collapse Menu
        el
            ~ cls "collapse navbar-collapse"
                @ att "id" "navbarSupportedContent"
            $ do
                -- Navigation Links
                ul ~ cls "navbar-nav me-auto mb-2 mb-lg-0" $ do
                    li ~ cls "nav-item"
                        $ link homeUrl
                            ~ cls "nav-link active"
                                @ att "aria-current" "page"
                        $ "Home"

                    li ~ cls "nav-item" $
                        link homeUrl ~ cls "nav-link" $
                            "Link"

                    li ~ cls "nav-item" $ colorModeToggle

                    -- Dropdown
                    li ~ cls "nav-item dropdown" $ do
                        link homeUrl
                            ~ cls "nav-link dropdown-toggle"
                                @ att "role" "button"
                                . att "data-bs-toggle" "dropdown"
                                . att "aria-expanded" "false"
                            $ "Dropdown"

                        ul ~ cls "dropdown-menu" $ do
                            li $ link homeUrl ~ cls "dropdown-item" $ "Action"
                            li $ link homeUrl ~ cls "dropdown-item" $ "Another action"
                            li $ tag "hr" ~ cls "dropdown-divider" $ none
                            li $ link homeUrl ~ cls "dropdown-item" $ "Something else here"

                    li ~ cls "nav-item"
                        $ link homeUrl
                            ~ cls "nav-link disabled"
                                @ att "aria-disabled" "true"
                        $ "Disabled"

                -- Search Form
                tag "form" ~ cls "d-flex" @ att "role" "search" $ do
                    tag "input"
                        ~ cls "form-control me-2"
                            @ att "type" "search"
                            . att "placeholder" "Search"
                            . att "aria-label" "Search"
                        $ none
                    tag "button"
                        ~ cls "btn btn-outline-success"
                            @ att "type" "submit"
                        $ "Search"
