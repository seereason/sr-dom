{-# language CPP, OverloadedStrings #-}
module Main where

import Data.Text (Text)
import SeeReason.DOM

main = runDOM app

app :: DOM ()
app = do
  doc <- document
  waitReady doc
  let css = ["button { background-color: green; }", "button:disabled { background-color: red; }"] :: [Text]
  styl <- style_ [] (cdatas css)
  b1 <- button_ [type_ "button"]            (cdata "I am active.")
  b2 <- button_ [type_ "button", disabled_] (cdata "I am disabled.")

  -- assemble
  -- ToDo  assemble, then attach, then activate. (hidden/ or display: none, or something)
  bod <- body doc
  bod <-- styl
  bod <-- b1
  bod <-- b2
  eventLoop


#if 0
tree :: Tree App
tree = div_ [ style_ CSS , button Active, button Disabled]
  where button Active = button_ [type_ "button"] (Description Active)
        button Disabled = button_ [type_ "button"] (Description Disabled)
#endif


#if 0
  <body>
  <style>
    button { background-color: green; }
    button:disabled { background-color: red; }
  </style>
  <button type="button">I am active.</button>
  <button type="button" disabled>I am disabled.</button>
</body>
#endif
