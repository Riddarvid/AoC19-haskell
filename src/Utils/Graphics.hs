module Utils.Graphics (renderDay13Start) where

import           Data.HashSet   (HashSet)
import qualified Data.HashSet   as HS
import           Day13          (GameState (gsScore, gsTiles), Tile (Tile),
                                 TileType (..), getStartTiles)
import           Graphics.Gloss (Color, Display (InWindow), Picture, black,
                                 blue, color, display, green, pictures,
                                 rectangleSolid, red, scale, text, translate,
                                 white)
import           Utils.Geometry (Point2 (P2))
import           Utils.Intcode  (Program)

renderDay13Start :: Program -> IO ()
renderDay13Start program = display (InWindow "Day13" (900, 900) (10, 10)) white mainPicture
  where
    gs = getStartTiles program
    tilesPicture = renderTiles $ gsTiles gs
    scorePicture = renderScore $ gsScore gs
    mainScale = 10
    mainPicture = scale mainScale mainScale $ tilesPicture <> scorePicture

renderScore :: Integer -> Picture
renderScore score = translate 0 (-10) $ scale textScale textScale $ text ("Score: " ++ show score)
  where
    textScale = 0.05

renderTiles :: HashSet Tile -> Picture
renderTiles tiles = scale tileScale tileScale $ pictures $ map renderTile $ HS.toList tiles
  where
    tileScale = 1

renderTile :: Tile -> Picture
renderTile (Tile (P2 x y) tileType) = translate x' y' $ color (tileColor tileType) square
  where
    x' = fromIntegral x
    y' = fromIntegral y

tileColor :: TileType -> Color
tileColor tileType = case tileType of
  Empty  -> white
  Wall   -> black
  Block  -> red
  Paddle -> green
  Ball   -> blue

square :: Picture
square = rectangleSolid 1 1
