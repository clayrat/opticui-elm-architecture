module Five where
--------------------------------------------------------------------------------
import Prelude
import Control.Monad.Aff
import Data.Foldable (mconcat)
import Data.JSON as JS
import Data.Lens
import Data.Lens.Index (ix)
import Data.Maybe
import OpticUI
import OpticUI.Markup.HTML as H
import Network.HTTP.Affjax as AJ
--------------------------------------------------------------------------------

type Viewer = { topic :: String, gifUrl :: String }

randomGiphy :: String -> String
randomGiphy t = mconcat
  [ "https://api.giphy.com/v1/gifs/random"
  , "?tag=", t
  , "&api_key=", "dc6zaTOxFJmzC"
  ]

main = animate { topic: "funny cats", gifUrl: "" } $ with \st h -> let
  loaded a = runHandler h $ st # gifUrl .~ (a ^. _Just
                                            <<< _JObject <<< ix "data"
                                            <<< _JObject <<< ix "image_url"
                                            <<< _JString)
  failed _ = runHandler h $ st # gifUrl .~ ""
  submit t = const $ do
    runAff failed loaded $ JS.decode <<< _.response <$> AJ.get (randomGiphy t)
  in ui $ H.div_ $ mconcat
  [ H.h2_ $ text st.topic
  , H.img [ H.srcA st.gifUrl ] $ Markup []
  , H.button [ H.onClick $ submit st.topic ] $ text "More Please!"
  ]

topic  = lens _.topic  (_ { topic  = _ })
gifUrl = lens _.gifUrl (_ { gifUrl = _ })

_JObject = prism' JS.JObject $ \x -> case x of
  JS.JObject y -> Just y
  _ -> Nothing
_JString = prism' JS.JString $ \x -> case x of
  JS.JString y -> Just y
  _ -> Nothing
