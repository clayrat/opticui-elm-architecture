module Two where
--------------------------------------------------------------------------------
import           Prelude hiding (top, bottom)
import           Data.Foldable       (mconcat)
import           Data.Lens
import           DOM          (DOM ())
import           OpticUI
import qualified OpticUI.Markup.HTML as H
--------------------------------------------------------------------------------

type Counter = { count :: Int }

init x = { count : x }

data Action  = Increment | Decrement

update Increment = count +~ 1
update Decrement = count -~ 1

counter = with $ \st h ->
  let clicked a = const $ runHandler h $ st # update a
  in ui $ H.div_ $ mconcat
  [ H.button [ H.onClick $ clicked $ Decrement ] $ text "-"
  , text $ show st.count
  , H.button [ H.onClick $ clicked $ Increment ] $ text "+"
  ]

-- new stuff

type Counter2 = { top :: Counter, bottom :: Counter}

init2 a b = { top : init a, bottom : init b }

data Action2 = Reset

update2 Reset = (top .~ init 0) <<< (bottom .~ init 0)

main = animate (init2 0 0) $ with \st h ->
  let clicked2 a = const $ runHandler h $ st # update2 a
  in mconcat $
  [ top    $ counter
  , bottom $ counter
  , ui $ H.button [ H.onClick $ clicked2 Reset ] $ text "RESET"
  ]

count  = lens _.count  (_ { count  = _ })
top    = lens _.top    (_ { top    = _ })
bottom = lens _.bottom (_ { bottom = _ })
