module Halogen.Canvas.Interact
  ( Interact(..)
  , InputEvent(..)
  , stopInputEventPropagation
  , component
  ) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Graphics.Canvas.Free (CanvasT, clearRect, getBoundingClientRect, getHeight, getWidth, withContext)
import Halogen as H
import Halogen.Canvas (Dimensions)
import Halogen.Canvas as Canvas
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Web.DOM.Element (DOMRect)
import Web.Event.Event (stopPropagation)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

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


type Slots m = ( canvas :: forall o. H.Slot (CanvasT m) o Unit ) 

_canvas = Proxy :: Proxy "canvas"


data Action =
    Initialize
  | InputEvent InputEvent 

component :: forall q o world m. MonadAff m => MonadRec m => H.Component q (Interact world m) o m
component = do
  H.mkComponent
    { initialState: identity 
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just Initialize
                                     }
    }

render :: forall world m. MonadAff m => MonadRec m => Interact world m -> H.ComponentHTML Action (Slots m) m
render { dimensions } =
  HH.div
    [ HE.onKeyDown (InputEvent <<< KeyDown) 
    , HE.onKeyUp (InputEvent <<< KeyUp)
    , HE.onClick (InputEvent <<< Click)
    , HE.onDoubleClick (InputEvent <<< DoubleClick)
    , HE.onMouseDown (InputEvent <<< MouseDown)
    , HE.onMouseUp (InputEvent <<< MouseUp)
    , HE.onMouseEnter (InputEvent <<< MouseEnter)
    , HE.onMouseLeave (InputEvent <<< MouseLeave)
    , HE.onMouseMove (InputEvent <<< MouseMove)
    ]
    [ HH.slot_ _canvas unit Canvas.component dimensions
    ]

handleAction :: forall m o world .
                MonadAff m
             => MonadRec m
             => Action
             -> H.HalogenM (Interact world m) Action (Slots m) o m Unit
handleAction = case _ of
  Initialize -> do
    { draw, world } <- H.get
    void $ H.query _canvas unit (withContext $ draw world )
  InputEvent e -> do
    rect <- H.query _canvas unit getBoundingClientRect
    flip traverse_ rect $ \r -> do
       H.liftEffect $ stopInputEventPropagation e
       { draw, world } <- H.modify (\st -> st { world = st.input e r st.world })
       void $ H.query _canvas unit do
          withContext do
            width <- getWidth
            height <- getHeight
            clearRect { x: 0.0, y: 0.0, width, height }
            draw world

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


