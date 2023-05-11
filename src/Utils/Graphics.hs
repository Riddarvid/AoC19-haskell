module Utils.Graphics (
  tick,
  square
) where

import           Data.Sequence  (Seq, ViewR ((:>)))
import qualified Data.Sequence  as Seq
import           Graphics.Gloss (Picture, rectangleSolid)

tick :: Float -> [Picture] -> (Float -> Picture)
tick interval = tick' interval . Seq.fromList

tick' :: Float -> Seq Picture -> (Float -> Picture)
tick' interval pics time = case Seq.lookup index pics of
  Just pic -> pic
  Nothing  -> case Seq.viewr pics of
    (_ :> pic) -> pic
    _          -> error "Sequence is empty"
  where
    index = floor $ time / interval

square :: Picture
square = rectangleSolid 1 1
