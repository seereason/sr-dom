
module SeeReason.DOM.Screen (getScreen, Screen(..)) where

import qualified GHCJS.DOM.Types as GD (Screen)
import qualified GHCJS.DOM as GD (getScreen)


data Screen = Screen deriving (Eq, Show, Generic)

getScreen :: Window -> DOM Screen
getScreen = DOM . liftIO . GD.getScreen
