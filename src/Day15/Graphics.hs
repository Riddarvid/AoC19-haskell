module Day15.Graphics (renderRobotSearch) where
import qualified Data.HashMap.Lazy as HM
import           Data.HashSet      (HashSet)
import qualified Data.HashSet      as HS
import           Day15             (Action (..), OxygenMap, Pos,
                                    RobotMove (RobotMove), Tile (..),
                                    exploreMapPath)
import           Graphics.Gloss    (Display (InWindow), Picture, animate, black,
                                    green, greyN, pictures, red, scale, white)
import           Utils.Graphics    (renderSquare, tick)
import           Utils.Intcode     (Program)

robotDisplay :: Display
robotDisplay = InWindow "Day15 - Robot" (900, 900) (10, 10)

data World = World {
  wDiscovered :: HashSet Pos,
  wRobot      :: Pos
}

renderRobotSearch :: Program -> IO ()
renderRobotSearch prg = animate robotDisplay white $ tick 0.01 pics
  where
    (oxygenMap, robotPath) = exploreMapPath prg
    worlds = reverse $ foldr worldFromPath [] robotPath
    pics = map (renderWorld oxygenMap) worlds

worldFromPath :: RobotMove -> [World] -> [World]
worldFromPath (RobotMove pos action) worlds = world' : worlds
  where
    world = case worlds of
      []      -> World {wDiscovered = HS.empty, wRobot = pos}
      (x : _) -> x
    world' = world{
      wDiscovered = HS.insert pos (wDiscovered world),
      wRobot = case action of
        Move    -> pos
        Explore -> wRobot world
    }

renderWorld :: OxygenMap -> World -> Picture
renderWorld oxygenMap world =
  scale 5 5
  (pictures (map (renderTile oxygenMap) $ HS.toList $ wDiscovered world) <>
  renderRobot (wRobot world))

renderTile :: OxygenMap -> Pos -> Picture
renderTile oxygenMap pos = renderSquare color' pos
  where
    color' = case HM.lookup pos oxygenMap of
      Nothing -> error "Pos not in map"
      Just tile -> case tile of
        Wall   -> black
        Floor  -> greyN 0.8
        Source -> green

renderRobot :: Pos -> Picture
renderRobot = renderSquare red
