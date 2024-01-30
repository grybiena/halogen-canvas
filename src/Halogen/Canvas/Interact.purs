module Halogen.Canvas.Interact
  ( InputEvent(..)
  , stopInputEventPropagation
  , component
  ) where

import Prelude

import Control.Monad.Reader (runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Graphics.Canvas (getContext2D)
import Graphics.Canvas.Free (CanvasContext, CanvasT, clearRect, getHeight, getWidth, runCanvasT, withContext)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (DOMRect, getBoundingClientRect)
import Web.Event.Event (stopPropagation)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

type Dimensions =
  { width :: Int
  , height :: Int
  }

data InputEvent =
    KeyDown KeyboardEvent
  | KeyUp KeyboardEvent
  | Click MouseEvent 
  | DoubleClick MouseEvent
  | MouseDown MouseEvent
  | MouseUp MouseEvent
  | MouseEnter MouseEvent
  | MouseLeave MouseEvent
  | MouseMove MouseEvent

type Interact world m =
  { dimensions :: Dimensions 
  , world :: world
  , draw :: world -> CanvasT m Unit
  , input :: InputEvent -> DOMRect -> world -> world
  }

type State world m =
  { interact :: Interact world m 
  , canvas :: Maybe CanvasContext
  }

data Action =
    Initialize
  | InputEvent InputEvent 

component :: forall q o world m. MonadAff m => MonadRec m => H.Component q (Interact world m) o m
component = do
  H.mkComponent
    { initialState: \interact -> { interact, canvas: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just Initialize
                                     }
    }

render :: forall world m. State world m -> H.ComponentHTML Action () m
render { interact } =
  HH.canvas
    [ HP.ref (H.RefLabel "canvas")
    , HP.width interact.dimensions.width
    , HP.height interact.dimensions.height
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
       let ctx = { canvasElement, context2D }
       { interact } <- H.modify (\st -> st { canvas = Just ctx })
       H.lift $ runReaderT (runCanvasT (withContext $ interact.draw interact.world )) ctx
  InputEvent e -> do
    l <- H.getRef (H.RefLabel "canvas")
    flip traverse_ l $ \ce -> do
       r <- H.liftEffect $ getBoundingClientRect ce
       H.liftEffect $ stopInputEventPropagation e
       { interact, canvas } <- H.modify (\st -> st { interact = st.interact { world = st.interact.input e r st.interact.world } })
       flip traverse_ canvas $ \c -> do
         let draw = do
               width <- getWidth
               height <- getHeight
               clearRect { x: 0.0, y: 0.0, width, height }
               interact.draw interact.world
         H.lift $ runReaderT (runCanvasT (withContext draw)) c


stopInputEventPropagation :: InputEvent -> Effect Unit
stopInputEventPropagation (KeyDown e) = stopPropagation $ KeyboardEvent.toEvent e
stopInputEventPropagation (KeyUp e) = stopPropagation $ KeyboardEvent.toEvent e
stopInputEventPropagation (Click e) = stopPropagation $ MouseEvent.toEvent e
stopInputEventPropagation (DoubleClick e) = stopPropagation $ MouseEvent.toEvent e
stopInputEventPropagation (MouseDown e) = stopPropagation $ MouseEvent.toEvent e
stopInputEventPropagation (MouseUp e) = stopPropagation $ MouseEvent.toEvent e
stopInputEventPropagation (MouseEnter e) = stopPropagation $ MouseEvent.toEvent e
stopInputEventPropagation (MouseLeave e) = stopPropagation $ MouseEvent.toEvent e
stopInputEventPropagation (MouseMove e) = stopPropagation $ MouseEvent.toEvent e


