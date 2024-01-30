module Halogen.Canvas.Game
  ( Game(..)
  , component
  ) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect.Aff.Class (class MonadAff)
import Graphics.Canvas.Free (CanvasT, clearRect, getBoundingClientRect, getHeight, getWidth, withContext)
import Halogen as H
import Halogen.Canvas (Dimensions)
import Halogen.Canvas as Canvas
import Halogen.Canvas.Interact (InputEvent(..), stopInputEventPropagation)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Subscription as HS
import Type.Prelude (Proxy(..))
import Web.DOM.Element (DOMRect)
import Web.HTML (window)
import Web.HTML.Window.AnimationFrame (DOMHighResTimestamp, requestAnimationFrame)

type Game world m =
  { dimensions :: Dimensions 
  , world :: world
  , draw :: world -> CanvasT m Unit
  , input :: InputEvent -> DOMRect -> world -> world
  , animate :: DOMHighResTimestamp -> world -> world
  }

type Slots m = ( canvas :: forall o. H.Slot (CanvasT m) o Unit ) 

_canvas = Proxy :: Proxy "canvas"

data Action =
    Initialize
  | AnimationFrame DOMHighResTimestamp
  | InputEvent InputEvent 

component :: forall q o world m. MonadAff m => MonadRec m => H.Component q (Game world m) o m
component = do
  H.mkComponent
    { initialState: identity 
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just Initialize
                                     }
    }

render :: forall world m. MonadAff m => MonadRec m => Game world m -> H.ComponentHTML Action (Slots m) m
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
             -> H.HalogenM (Game world m) Action (Slots m) o m Unit
handleAction = case _ of
  Initialize -> do
    { draw, world } <- H.get
    { emitter, listener } <- H.liftEffect HS.create
    void $ H.query _canvas unit (withContext $ draw world )
    let animationLoop t = do
          HS.notify listener t
          void $ window >>= requestAnimationFrame animationLoop
    H.liftEffect $ void $ window >>= requestAnimationFrame animationLoop
    void $ H.subscribe (AnimationFrame <$> emitter)
  InputEvent e -> do
    rect <- H.query _canvas unit getBoundingClientRect 
    flip traverse_ rect $ \r -> do
      H.liftEffect $ stopInputEventPropagation e
      H.modify_ (\st -> st { world = st.input e r st.world })
  AnimationFrame t -> do
    { draw, world } <- H.modify (\st -> st { world = st.animate t st.world })
    void $ H.query _canvas unit do
       withContext do
         width <- getWidth
         height <- getHeight
         clearRect { x: 0.0, y: 0.0, width, height }
         draw world

