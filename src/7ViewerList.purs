module Seven where
--------------------------------------------------------------------------------
import Prelude
import Control.Monad.Aff (runAff)
import Data.Foldable (mconcat)
import Data.Lens
import Data.Lens.Index (ix)
import Data.List hiding (init)
import Data.Maybe
import Data.Monoid

import DOM (DOM ())
import Data.JSON as JS
import Network.HTTP.Affjax as AJ
import OpticUI
import OpticUI.Components
import OpticUI.Markup.HTML as H
--------------------------------------------------------------------------------

_JObject = prism' JS.JObject $ \x -> case x of
  JS.JObject y -> Just y
  _ -> Nothing
_JString = prism' JS.JString $ \x -> case x of
  JS.JString y -> Just y
  _ -> Nothing

type GifViewer = { topic :: String, gifUrl :: String }
topic  = lens _.topic  (_ { topic  = _ })
gifUrl = lens _.gifUrl (_ { gifUrl = _ })

init :: String -> GifViewer
init t = { topic : t , gifUrl : "" }

randomGiphy :: String -> String
randomGiphy t = mconcat
  [ "https://api.giphy.com/v1/gifs/random"
  , "?tag=", t
  , "&api_key=", "dc6zaTOxFJmzC"
  ]

extractUrl :: Maybe JS.JValue -> String
extractUrl a = a ^. _Just
             <<< _JObject <<< ix "data"
             <<< _JObject <<< ix "image_url"
             <<< _JString

viewer :: forall eff. UI (dom :: DOM, ajax :: AJ.AJAX| eff) Markup GifViewer GifViewer
viewer = with \st h -> let
  setUrl u = runHandler h $ st # gifUrl .~ u
  more = runAff
          (const $ setUrl "error")
          (_.response >>> JS.decode >>> extractUrl >>> setUrl)
          (AJ.get (randomGiphy st.topic))
  in ui $ H.div_ $ mconcat
  [ H.h2_ $ text st.topic
  , H.div [ H.onInitialized st.topic $ const more] $ H.img
      [ H.srcA st.gifUrl
      , H.heightA 200, H.widthA 200 ] mempty
  , H.button [ H.onClick $ const more ] $ text "More Please!"
  ]

 -- new stuff

type ViewerL = { input :: String, viewers :: List GifViewer }
viewers = lens _.viewers (_ { viewers = _ })
input =   lens _.input   (_ { input   = _ })

main = animate { input : "", viewers : Nil } $ with $ \s h -> let
    addOnEnter ev (Just t)  = if (ev.keyCode == 13) then
                                runHandler h $ s # (viewers %~ ((flip snoc) $ init t)) <<< (input .~ "")
                              else pure unit
    addOnEnter _   Nothing  = pure unit
  in mconcat
  [ input $ textField [H.onKeydown addOnEnter]
  , withView (H.div [ H.styleA "display: flex;" ]) (viewers $ foreach $ const viewer)
  ]
