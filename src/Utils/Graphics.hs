module Utils.Graphics (renderDay13Start) where

import qualified Data.HashSet   as HS
import           Day13          (Tile (Tile), TileType (..), getStartTiles)
import           Graphics.Gloss (Color, Display (InWindow), Picture, black,
                                 blue, color, display, green, pictures,
                                 rectangleSolid, red, scale, translate, white)
import           Utils.Geometry (Point2 (P2))
import           Utils.Intcode  (Program)

renderDay13Start :: Program -> IO ()
renderDay13Start program = display (InWindow "Day13" (900, 900) (10, 10)) white picture
  where
    tiles = HS.toList $ getStartTiles program
    sizeFactor = 10
    picture = scale sizeFactor sizeFactor $ pictures $ map renderTile tiles

renderTile :: Tile -> Picture
renderTile (Tile (P2 x y) tileType) = translate x' y' $ color (tileColor tileType) square
  where
    x' = spaceFactor * fromIntegral x
    y' = spaceFactor * fromIntegral y
    spaceFactor = 1

tileColor :: TileType -> Color
tileColor tileType = case tileType of
  Empty  -> white
  Wall   -> black
  Block  -> red
  Paddle -> green
  Ball   -> blue

square :: Picture
square = rectangleSolid side side
  where
    side = 1
