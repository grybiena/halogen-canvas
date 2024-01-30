module Halogen.Canvas.Animate
  ( component
  ) where

import Prelude

import Control.Monad.Reader (runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect.Aff.Class (class MonadAff)
import Graphics.Canvas (getContext2D)
import Graphics.Canvas.Free (CanvasContext, CanvasT, clearRect, getHeight, getWidth, runCanvasT, withContext)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.Window.AnimationFrame (DOMHighResTimestamp, requestAnimationFrame)

type Dimensions =
  { width :: Int
  , height :: Int
  }


type Animation m =
  { dimensions :: Dimensions
  , animation :: DOMHighResTimestamp -> CanvasT m Unit
  }

type State m =
  { dimensions :: Dimensions
  , animation :: DOMHighResTimestamp -> CanvasT m Unit
  , canvas :: Maybe CanvasContext
  }

data Action =
    Initialize
  | AnimationFrame DOMHighResTimestamp

component :: forall q o m. MonadAff m => MonadRec m => H.Component q (Animation m) o m
component = do
  H.mkComponent
    { initialState: \{ dimensions, animation } -> { dimensions, animation, canvas: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just Initialize
                                     }
    }

render :: forall m. State m -> H.ComponentHTML Action () m
render { dimensions } =
  HH.canvas
    [ HP.ref (H.RefLabel "canvas")
    , HP.width dimensions.width
    , HP.height dimensions.height
    ]

handleAction :: forall m o .
                MonadAff m
             => MonadRec m
             => Action
             -> H.HalogenM (State m) Action () o m Unit
handleAction = case _ of
  Initialize -> do
    e <- H.getRef (H.RefLabel "canvas")
    flip traverse_ e $ \ce -> do
       let canvasElement = unsafeCoerce ce
       context2D <- H.liftEffect $ getContext2D canvasElement
       H.modify_ (\st -> st { canvas = Just { canvasElement, context2D } })
       { emitter, listener } <- H.liftEffect HS.create
       let animationLoop t = do
             HS.notify listener t
             void $ window >>= requestAnimationFrame animationLoop
       H.liftEffect $ void $ window >>= requestAnimationFrame animationLoop
       void $ H.subscribe (AnimationFrame <$> emitter)
  AnimationFrame t -> do
    { animation, canvas } <- H.get
    flip traverse_ canvas $ \c -> do
      let draw = do
            width <- getWidth
            height <- getHeight
            clearRect { x: 0.0, y: 0.0, width, height }
            animation t
      H.lift $ runReaderT (runCanvasT (withContext draw)) c


