module One where
--------------------------------------------------------------------------------
import           Prelude
import           Data.Foldable       (mconcat)
import           Data.Lens
import           OpticUI
import qualified OpticUI.Markup.HTML as H
--------------------------------------------------------------------------------

type Counter = { count :: Int }

data Action  = Increment | Decrement

update Increment = count +~ 1
update Decrement = count -~ 1

counter = with $ \st h ->
  let clicked a = const $ runHandler h $ st # update a
  in ui $ H.div_ $ mconcat
  [ H.button [ H.onClick $ clicked Decrement ] $ text "-"
  , text $ show st.count
  , H.button [ H.onClick $ clicked Increment ] $ text "+"
  ]

main = animate { count : 0 } $ counter

count = lens _.count (_ { count = _ })
