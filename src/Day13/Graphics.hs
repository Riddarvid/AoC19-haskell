module Day13.Graphics (
  renderDay13part1,
  renderDay13part2
) where
import           AoCUtils.Geometry   (Point2)
import           AoCUtils.Graphics   (renderSquare, tick)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Day13               (GameState (gsScore, gsTiles), Tile (..),
                                      getStartTiles, runGame)
import           Graphics.Gloss      (Color, Display (InWindow), Picture,
                                      animate, black, blue, display, green,
                                      pictures, red, scale, text, translate,
                                      white)
import           Utils.Intcode       (Program)

day13Display :: Display
day13Display = InWindow "Day13" (900, 900) (10, 10)

renderDay13part1 :: Program -> IO ()
renderDay13part1 program = display day13Display white picture
  where
    gs = getStartTiles program
    picture = renderGS gs

renderDay13part2 :: Program -> IO ()
renderDay13part2 program = animate day13Display white $ tick 0.01 pics
  where
    states = runGame program
    pics = map renderGS states

-- General

renderGS :: GameState -> Picture
renderGS gs = scale mainScale mainScale $ tilesPicture <> scorePicture
  where
    tilesPicture = renderTiles $ gsTiles gs
    scorePicture = renderScore $ gsScore gs
    mainScale = 10

renderScore :: Integer -> Picture
renderScore score = translate 0 2 $ scale textScale textScale $ text ("Score: " ++ show score)
  where
    textScale = 0.05

renderTiles :: HashMap (Point2 Integer) Tile -> Picture
renderTiles tiles = scale tileScale tileScale $ pictures $ map (uncurry renderTile) $ HM.toList tiles
  where
    tileScale = 1

renderTile :: Point2 Integer -> Tile -> Picture
renderTile p tile = renderSquare (tileColor tile) p

tileColor :: Tile -> Color
tileColor tileType = case tileType of
  Empty  -> white
  Wall   -> black
  Block  -> red
  Paddle -> green
  Ball   -> blue
