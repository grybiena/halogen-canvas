module Halogen.Canvas.Game
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
import Halogen.Canvas.Interact (InputEvent(..), stopInputEventPropagation)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (DOMRect, getBoundingClientRect)
import Web.HTML (window)
import Web.HTML.Window.AnimationFrame (DOMHighResTimestamp, requestAnimationFrame)

type Dimensions =
  { width :: Int
  , height :: Int
  }


type Game world m =
  { dimensions :: Dimensions 
  , world :: world
  , draw :: world -> CanvasT m Unit
  , input :: InputEvent -> DOMRect -> world -> world
  , animate :: DOMHighResTimestamp -> world -> world
  }

type State world m =
  { game :: Game world m 
  , canvas :: Maybe CanvasContext
  }

data Action =
    Initialize
  | AnimationFrame DOMHighResTimestamp
  | InputEvent InputEvent 

component :: forall q o world m. MonadAff m => MonadRec m => H.Component q (Game world m) o m
component = do
  H.mkComponent
    { initialState: \game -> { game, canvas: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just Initialize
                                     }
    }

render :: forall world m. State world m -> H.ComponentHTML Action () m
render { game } =
  HH.canvas
    [ HP.ref (H.RefLabel "canvas")
    , HP.width game.dimensions.width
    , HP.height game.dimensions.height
    , HE.onKeyDown (InputEvent <<< KeyDown) 
    , HE.onKeyUp (InputEvent <<< KeyUp)
    , HE.onClick (InputEvent <<< Click)
    , HE.onDoubleClick (InputEvent <<< DoubleClick)
    , HE.onMouseDown (InputEvent <<< MouseDown)
    , HE.onMouseUp (InputEvent <<< MouseUp)
    , HE.onMouseEnter (InputEvent <<< MouseEnter)
    , HE.onMouseLeave (InputEvent <<< MouseLeave)
    , HE.onMouseMove (InputEvent <<< MouseMove)
    ]

handleAction :: forall m o world .
                MonadAff m
             => MonadRec m
             => Action
             -> H.HalogenM (State world m) Action () o m Unit
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
  InputEvent e -> do
    c <- H.getRef (H.RefLabel "canvas")
    flip traverse_ c $ \ce -> do
       r <- H.liftEffect $ getBoundingClientRect ce
       H.liftEffect $ stopInputEventPropagation e
       H.modify_ (\st -> st { game = st.game { world = st.game.input e r st.game.world } })
  AnimationFrame t -> do
    { game, canvas } <- H.modify (\st -> st { game = st.game { world = st.game.animate t st.game.world } })
    flip traverse_ canvas $ \c -> do
      let draw = do
            width <- getWidth
            height <- getHeight
            clearRect { x: 0.0, y: 0.0, width, height }
            game.draw game.world
      H.lift $ runReaderT (runCanvasT (withContext draw)) c


