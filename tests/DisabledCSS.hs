{-# language CPP, OverloadedStrings #-}
module Main where

import Data.Text (Text)
import Data.Tree
import SeeReason.DOM
import SeeReason.DOM.GHCJSTypes (Document, Element)

main = runDOM app

app :: DOM ()
app = do
  doc <- document
  waitReady doc
  let css = ["button { background-color: green; }", "button:disabled { background-color: red; }"] :: [Text]
  let s = style_ [] (cdatas css)
      b1 = button_ [type_ "button"]            (cdatas ["I am active."])
      b2 = button_ [type_ "button", disabled_] (cdatas ["I am disabled."])
      root = div_ [] [s,b1,b2]

  -- assemble
  -- ToDo  assemble, then attach, then activate. (hidden/ or display: none, or something)
  bod <- body doc
  rr <- realize doc root
  bod <-- rr
  waitForever

realize :: Document -> Html -> DOM Element
realize doc h = do
  case h of
    Node (CData t) [] -> createTextNode doc t
    Node (CData t) _ -> error "realize got CData with children, disallow statically"
    Node (El tg as) cs -> do
      e <- createElement doc tg
      mapM_ (\(k,v) -> setAttribute e k v) as
      cs' <- mapM (realize doc) cs
      mapM_ (appendChild  e) cs'
      return e

(<--) = appendChild

button_ = tag "button"
type_ = attr "type"
disabled_ = attr "disabled" ""
div_ = tag "div"

tag t as cs = Node (El t as) cs
attr k v = (k,v)

data El = El Text Attrs | CData Text deriving (Show)

cdatas :: [Text] -> [Html]
cdatas = map cdata
cdata :: Text -> Html
cdata t = Node (CData t) []

type Attr = (Text, Text)
type Attrs = [Attr]
type Html = Tree El

style_ :: [(Text,Text)] -> [Html] -> Html
style_ = tag "style"

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
