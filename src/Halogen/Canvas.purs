module Halogen.Canvas
  ( Output(..)
  , component
--  , module CanvasFree
  ) where

import Prelude

import Control.Monad.Reader (runReaderT)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Effect.Aff.Class (class MonadAff)
import Graphics.Canvas (getContext2D)
import Halogen as H
import Halogen.Canvas.Free (Canvas, CanvasM, runCanvas)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Unsafe.Coerce (unsafeCoerce)

type State =
  { canvas :: Maybe Canvas
  }

data Action = 
    Initialize
  | Finalize
  | Raise Output

data Output =
    Key 
  | Resize 

component :: forall i m. MonadAff m => H.Component CanvasM i Output m
component = do
  H.mkComponent
    { initialState: const { canvas: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , handleQuery = handleQuery 
                                     , initialize = Just Initialize
                                     , finalize = Just Finalize
                                     }
    }


render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render _ = HH.canvas [ HP.ref (H.RefLabel "canvas")]


handleAction :: forall m .
                MonadAff m
             => Action
             -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    e <- H.getRef (H.RefLabel "canvas")
    flip traverse_ e $ \ce -> do
       let canvasElement = unsafeCoerce ce
       context2D <- H.liftEffect $ getContext2D canvasElement
       H.modify_ (\st -> st { canvas = Just { canvasElement, context2D } })
  Finalize -> pure unit
  Raise o -> H.raise o


handleQuery :: forall m a .
                MonadAff m
             => CanvasM a
             -> H.HalogenM State Action () Output m (Maybe a)
handleQuery f = do 
  { canvas } <- H.get
  map join $ flip traverse canvas $ \c -> do
    H.liftAff $ Just <$> runReaderT (runCanvas f) c


