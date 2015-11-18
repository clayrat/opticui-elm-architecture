module Four where
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

--counter d :: () forall eff. UI (dom :: DOM | eff) Markup Counter Counter
counterR d = with $ \st h ->
  let clicked a = const $ runHandler h $ st # update a
  in ui $ H.div_ $ mconcat
  [ H.button [ H.onClick $ clicked Decrement ] $ text "-"
  , text $ show st.count
  , H.button [ H.onClick $ clicked Increment ] $ text "+"
  , H.button [ H.onClick $ d ] $ text "X"
  ]

-- new stuff

type CounterLR = { counters :: List Counter }

data ActionLR = Insert | Remove Int

updateLR :: ActionLR -> CounterLR -> CounterLR
updateLR Insert     = counters %~ ((flip snoc) $ init 0)
updateLR (Remove i) = counters %~ deleteAt i >>> maybe Nil id

main = animate { counters : Nil } $ with \st h ->
  let handleLR a = const $ runHandler h $ st # updateLR a
  in mconcat
  [ ui $ H.button [ H.onClick $ handleLR Insert ] $ text "Add"
  , counters $ foreach (\i -> counterR <<< handleLR $ Remove i)
  ]

count    = lens _.count    (_ { count    = _ })
counters = lens _.counters (_ { counters = _ })
