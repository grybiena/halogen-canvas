module Halogen.Canvas.Free where

import Prelude

import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Rec.Class (class MonadRec)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Graphics.Canvas (CanvasElement, Context2D, fillText, getCanvasHeight, getCanvasWidth)

type Canvas =
  { canvasElement :: CanvasElement
  , context2D :: Context2D
  }

runCanvas :: forall m a .
               MonadAff m
            => MonadRec m
            => CanvasM a
            -> ReaderT Canvas m a
runCanvas = runFreeM go
  where
    go (Width a) = do
      { canvasElement } <- ask
      liftEffect $ a <$> getCanvasWidth canvasElement
    go (Height a) = do
      { canvasElement } <- ask
      liftEffect $ a <$> getCanvasHeight canvasElement
    go (FillText txt w h a) = do
      { context2D } <- ask
      liftEffect $ fillText context2D txt w h
      pure a

data CanvasF a =
    Width (Number -> a)
  | Height (Number -> a)
  | FillText String Number Number a

instance Functor CanvasF where
  map f (Width e) = Width (f <<< e)
  map f (Height e) = Height (f <<< e)
  map f (FillText txt w h e) = FillText txt w h (f e)

type CanvasM = Free CanvasF 

rows :: CanvasM Number
rows = liftF $ Width identity

cols :: CanvasM Number
cols = liftF $ Height identity

