import Graphics.Rendering.OpenGL.GL.Tensor
import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Interface.Pure.Game hiding (Point)
import Control.Applicative
import Data.Proxy
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Utilities

nub' :: Ord a => [a] -> [a]
nub' = Set.toList . Set.fromList

-- For nice math on vectors

type Point = Vector2 Int
type PointF = Vector2 Float

instance Num a => Num (Vector2 a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  abs = liftA abs
  signum = liftA signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (Vector2 a) where
  fromRational = pure . fromRational
  (/) = liftA2 (/)

fromT :: (a, a) -> Vector2 a
fromT = uncurry Vector2

toT :: Vector2 a -> (a, a)
toT (Vector2 x y) = (x, y)

-- Cellular automata

type World a = Map Point a

-- | Class of infinite neighboured cellular automata.
class Automaton a where
  -- | Neighborhood which affects the cell.
  -- | For now it should be symmetrical over X and Y axes.
  neighborhood :: Proxy a -> [Point]
  -- | Rule for the new state of the cell.
  step :: (Point -> Maybe a) -> Maybe a
  -- | Color of the cell.
  cellColor :: a -> Color
  -- | Default value of a cell.
  defaultCell :: a

-- This all is a comonad, but not a Control.Comonad

evaluate :: forall a. Automaton a => World a -> World a
evaluate w = Map.fromList $ mapMaybe update $ nub' $ concat $ map (enlist . fst) $ Map.toList w
  where
    enlist c = map (+ c) $ neighborhood (Proxy :: Proxy a)
    update c = (c, ) <$> step (\r -> Map.lookup (r + c) w)

-- Conway's Game of Life

data Conway's = Conway's

instance Automaton Conway's where
  neighborhood _ = [ Vector2 x y | x <- [-1..1], y <- [-1..1] ]
  step f = if c' then Just Conway's else Nothing
    where nb = filter (/= Vector2 0 0) $ neighborhood (Proxy :: Proxy Conway's)
          c = isJust $ f $ Vector2 0 0
          r = sum $ map (maybe 0 (const 1) . f) nb :: Int
          c'
            | c && r < 2 = False
            | c && r > 3 = False
            | not c && r == 3 = True
            | otherwise = c
  cellColor _ = white
  defaultCell = Conway's

-- Graphics

data Game a = Game PointF Bool (World a)

draw :: Automaton a => PointF -> Game a -> Picture
draw p (Game o _ wld) = uncurry Scale (toT p) $ trans o $ Pictures pics
  where pics = map (\(c, a) -> Color (cellColor a) $ trans (liftA fromIntegral c) px) $ Map.toList wld
        px = Polygon [(-0.5, -0.5), (0.5, -0.5), (0.5, 0.5), (-0.5, 0.5)]
        trans = uncurry Translate . toT

events :: Automaton a => PointF -> Event -> Game a -> Game a
events _ (EventKey (SpecialKey k) Down _ _) (Game o@(Vector2 x y) s w)
  | k == KeyLeft = Game (Vector2 (x + 1) y) s w
  | k == KeyRight = Game (Vector2 (x - 1) y) s w
  | k == KeyDown = Game (Vector2 x (y + 1)) s w
  | k == KeyUp = Game (Vector2 x (y - 1)) s w
  | k == KeySpace = Game o (not s) w
events p (EventKey (MouseButton LeftButton) Down _ (fromT -> c)) (Game o s w) = Game o s $ Map.alter switch k w
  where k = liftA round $ (o + c / p)
        switch = maybe (Just defaultCell) (const Nothing)
events _ _ w = w

advance :: Automaton a => Float -> Game a -> Game a
advance _ (Game o p g) = Game o p $ if p then evaluate g else g

main :: IO ()
main = play (InWindow "Game of Life" size (0, 0)) bcolor fps (Game (Vector2 0 0) True game) (draw csize) (events csize) advance
  where size = (640, 480)
        csize = Vector2 8 8
        bcolor = black
        fps = 4
        game = Map.empty :: World Conway's
