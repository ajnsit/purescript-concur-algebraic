module Concur.Algebraic where

import Concur.Core.Types as Concur
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Alternative ((<|>))
import Control.Applicative (pure, (*>))
import Control.Bind (bind)
import Control.Category ((<<<))
import Control.Monad (class Monad)
import Control.Monad.Free as Free
import Data.CommutativeRing ((+))
import Data.Functor (map)
import Data.Functor.Variant (VariantF)
import Data.Functor.Variant as Variant
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.Void (Void, absurd)
import Run (Run(..))
import Run as Run
import Run.Choose (CHOOSE)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

type WIDGET r = (widget :: Concur.Widget HTML | r)
_widget = Proxy :: Proxy "widget"

liftWidget :: forall r a. Concur.Widget HTML a -> Run (WIDGET + r) a
liftWidget = Run.lift _widget

mapWidgetF :: forall r a. (Concur.Widget HTML a -> Concur.Widget HTML a) -> Variant.VariantF (WIDGET + r) a -> Variant.VariantF (WIDGET + r) a
mapWidgetF f = Variant.overOne _widget f expandV
  where
  -- TODO: Figure out why Variant.expand doesn't work here
  --       I've tried adding the `Lacks "widget" r` constraint
  --       Perhaps I need to add the union constraint to all widget functions (yuck)
  expandV :: Variant.VariantF r a -> Variant.VariantF (WIDGET + r) a
  expandV = unsafeCoerce -- Variant.expand

-- TODO: Add this to purescript-run
runWrap
  :: forall a r
   . VariantF r (Run r a)
  -> Run r a
runWrap = Run <<< Free.wrap <<< coerce

shiftMapWidget :: forall r a. (forall b. (a -> b) -> Concur.Widget HTML b -> Concur.Widget HTML b) -> Run (WIDGET + r) a -> Run (WIDGET + r) a
shiftMapWidget f = Run.resume (runWrap <<< mapWidgetF (f pure)) pure

button :: forall r a. Array (P.ReactProps a) -> Run (WIDGET + r) a -> Run (WIDGET + r) a
button props = shiftMapWidget (\f w -> D.button_ (map (map f) props) w)

text :: forall r a. String -> Run (WIDGET + r) a
text = liftWidget <<< D.text

-- Yield
type YIELD x r = (yield :: Tuple x | r)
_yield = Proxy :: Proxy "yield"

liftYield :: forall r x a. Tuple x a -> Run (YIELD x + r) a
liftYield = Run.lift _yield

yield :: forall r x. x -> Run (YIELD x + r) Unit
yield x = liftYield (Tuple x unit)

handle :: forall x r a. Run (YIELD x + r) a -> (x -> Run (YIELD x + r) a -> Run (YIELD x + r) a) -> Run (YIELD x + r) a
handle w f = Run.run (Variant.on _yield (\ (Tuple x a) -> pure (f x a)) (Run.send <<< expandV)) w
  where
  -- TODO: Figure out why Variant.expand doesn't work here
  expandV :: forall b. Variant.VariantF r b -> Variant.VariantF (YIELD x + r) b
  expandV = unsafeCoerce -- Variant.expand

handleAndFinish :: forall x r. Run (YIELD x + r) Void -> Run r (Tuple x (Run (YIELD x + r) Void))
handleAndFinish w = Run.resume (Variant.on _yield pure (runWrap <<< coerce)) absurd w

--------------------------------------------------------------------------------
-- Sample program
--------------------------------------------------------------------------------

-- TODO: MonadRec etc.
forever :: forall m a. Monad m => m Unit -> m a
forever x = x *> forever x

-- A long running counter
counter :: forall r a. Run (WIDGET + r) a
counter = counter' 1
  where
  counter' n = do
    _ <- button [ P.onClick ] (text (show n))
    counter' (n+1)

-- paired with a widget that needs to send stuff out.
stuff :: forall r a. Run (WIDGET + YIELD Unit + r) a
stuff = forever do
  _ <- button [ P.onClick ] (text "show modal")
  yield unit

-- 1. Gets events from stuff, even though stuff is in parallel with counter, without counter losing its state
-- 2. Wrapper maintains the state and interacts with stuff to update it
-- 3. You are not forced to wrap directly at the parent of stuff. It can even wrap the entire rest of the application
-- 4. Wrapper' calls wrapper' again after handling events. If that call is not present, only the first yield will be handled
wrapper :: forall r a. Run (CHOOSE + WIDGET + r) a
wrapper = wrapper' 0 (counter <|> stuff)
  where
  wrapper' n cont = do
    Tuple _ cont' <- text (show n) <|> handleAndFinish cont
    wrapper' (n+1) cont'
