{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module YesodHelper(
    bootstrapCDN
  , bootstrapLayout
) where

import Yesod
import Data.Text (Text)

bootstrapCDN :: Text
bootstrapCDN = "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.0"

bootstrapLayout
  :: (Yesod a)
  => GWidget sub a ()
  -> GHandler sub a RepHtml
bootstrapLayout widget = do
    pc <- widgetToPageContent widget
    mmsg <- getMessage
    hamletToRepHtml [hamlet|
      $doctype 5
      <html>
        <head>
          <title>#{pageTitle pc}
          ^{pageHead pc}
        <body>
          <div .container>
            $maybe msg <- mmsg
              #{msg}
            ^{pageBody pc}
    |]
