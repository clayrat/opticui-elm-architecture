module Three where
--------------------------------------------------------------------------------
import Prelude hiding (top, bottom)
import Data.Foldable (mconcat)
import Data.Lens
import Data.List hiding (init)
import Data.Maybe (maybe)
import DOM (DOM ())
import OpticUI
import OpticUI.Markup.HTML as H
--------------------------------------------------------------------------------

type Counter = { count :: Int }

init :: Int -> Counter
init x = { count : x }

data Action = Increment | Decrement

update :: Action -> Counter -> Counter
update Increment = count +~ 1
update Decrement = count -~ 1

counter :: forall eff. UI (dom :: DOM | eff) Markup Counter Counter
counter = with $ \st h ->
  let clicked a = const $ runHandler h $ st # update a
  in ui $ H.div_ $ mconcat
  [ H.button [ H.onClick $ clicked Decrement ] $ text "-"
  , text $ show st.count
  , H.button [ H.onClick $ clicked Increment ] $ text "+"
  ]

-- new stuff

type CounterL = { counters :: List Counter }

data ActionL = Insert | Remove

updateL :: ActionL -> CounterL -> CounterL
updateL Remove = counters %~ tail >>> maybe Nil id
updateL Insert = counters %~ ((flip snoc) $ init 0)

main = animate { counters : Nil } $ with \st h ->
  let clickedL a = const $ runHandler h $ st # updateL a
  in mconcat
  [ ui $ H.button [ H.onClick $ clickedL Remove ] $ text "Remove"
  , ui $ H.button [ H.onClick $ clickedL Insert ] $ text "Add"
  , counters $ foreach $ const counter
  ]

count    = lens _.count    (_ { count    = _ })
counters = lens _.counters (_ { counters = _ })
