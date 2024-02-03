module Halogen.Canvas.Interact
  ( component
  , Output(..)
  , KeyInput(..)
  , MouseInput(..)
  ) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Graphics.Canvas.Free (CanvasT, getBoundingClientRect)
import Halogen as H
import Halogen.Canvas (Dimensions)
import Halogen.Canvas as Canvas
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Web.DOM.Element (DOMRect)
import Web.Event.Event (Event, stopPropagation)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

data KeyInput =
    KeyDown KeyboardEvent
  | KeyUp KeyboardEvent

data MouseInput =
    Click MouseEvent 
  | DoubleClick MouseEvent
  | MouseDown MouseEvent
  | MouseUp MouseEvent
  | MouseEnter MouseEvent
  | MouseLeave MouseEvent
  | MouseMove MouseEvent

data Output =
    KeyEvent KeyInput
  | MouseEvent MouseInput DOMRect

type Slots m = ( canvas :: forall o. H.Slot (CanvasT m) o Unit ) 

_canvas = Proxy :: Proxy "canvas"

data Action =
    KeyInput KeyInput 
  | MouseInput MouseInput

component :: forall m. MonadAff m => MonadRec m => H.Component (CanvasT m) Dimensions Output m
component = do
  H.mkComponent
    { initialState: identity 
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , handleQuery = H.query _canvas unit
                                     }
    }

render :: forall m. MonadAff m => MonadRec m => Dimensions -> H.ComponentHTML Action (Slots m) m
render dimensions =
  HH.div
    [ HE.onKeyDown (KeyInput <<< KeyDown) 
    , HE.onKeyUp (KeyInput <<< KeyUp)
    , HE.onClick (MouseInput <<< Click)
    , HE.onDoubleClick (MouseInput <<< DoubleClick)
    , HE.onMouseDown (MouseInput <<< MouseDown)
    , HE.onMouseUp (MouseInput <<< MouseUp)
    , HE.onMouseEnter (MouseInput <<< MouseEnter)
    , HE.onMouseLeave (MouseInput <<< MouseLeave)
    , HE.onMouseMove (MouseInput <<< MouseMove)
    ]
    [ HH.slot_ _canvas unit Canvas.component dimensions
    ]

handleAction :: forall m .
                MonadAff m
             => MonadRec m
             => Action
             -> H.HalogenM Dimensions Action (Slots m) Output m Unit
handleAction = case _ of
  KeyInput e -> do
    H.liftEffect $ stopInputEventPropagation e
    H.raise $ KeyEvent e
  MouseInput e -> do
    H.liftEffect $ stopInputEventPropagation e
    rect <- H.query _canvas unit getBoundingClientRect
    flip traverse_ rect $ \r -> do
       H.raise $ MouseEvent e r

class IsEvent e where 
  toEvent :: e -> Event

instance IsEvent KeyInput where
  toEvent (KeyDown e) = KeyboardEvent.toEvent e
  toEvent (KeyUp e) = KeyboardEvent.toEvent e

instance IsEvent MouseInput where
  toEvent (Click e) = MouseEvent.toEvent e
  toEvent (DoubleClick e) = MouseEvent.toEvent e
  toEvent (MouseDown e) = MouseEvent.toEvent e
  toEvent (MouseUp e) = MouseEvent.toEvent e
  toEvent (MouseEnter e) = MouseEvent.toEvent e
  toEvent (MouseLeave e) = MouseEvent.toEvent e
  toEvent (MouseMove e) = MouseEvent.toEvent e

stopInputEventPropagation :: forall e. IsEvent e => e -> Effect Unit
stopInputEventPropagation e = stopPropagation $ toEvent e

