import Linear.V2
import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Interface.Pure.Game hiding (Point)
import Control.Applicative
import Data.Maybe
import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Utilities

nub' :: Ord a => [a] -> [a]
nub' = Set.toList . Set.fromList

type Point = V2 Int
type PointF = V2 Float

-- Cellular automata

type World a = Map Point a

-- | Class of infinite neighboured cellular automata.
class Automaton a where
  -- | Neighborhood which affects the cell.
  -- | For now it should be symmetrical over X and Y axes.
  neighborhood :: a -> [Point]
  -- | Rule for the new state of the cell.
  step :: (Point -> Maybe a) -> Maybe a
  -- | Color of the cell.
  cellColor :: a -> Color
  -- | Default value of a cell.
  defaultCell :: a

-- This all is a comonad, but not a Control.Comonad

evaluate :: forall a. Automaton a => World a -> World a
evaluate w = Map.fromList $ mapMaybe update $ nub' $ concat $ map (uncurry enlist) $ Map.toList w
  where
    enlist c a = map (+ c) $ neighborhood a
    update c = (c, ) <$> step (\r -> Map.lookup (r + c) w)

-- Conway's Game of Life

data Conway's = Conway's

instance Automaton Conway's where
  neighborhood _ = [ V2 x y | x <- [-1..1], y <- [-1..1] ]
  step f = if c' then Just Conway's else Nothing
    where nb = filter (/= V2 0 0) $ neighborhood Conway's
          c = isJust $ f $ V2 0 0
          r = sum $ map (maybe 0 (const 1) . f) nb :: Int
          c'
            | c && r < 2 = False
            | c && r > 3 = False
            | not c && r == 3 = True
            | otherwise = c
  cellColor _ = white
  defaultCell = Conway's

-- Nice shading

data Shaded a = Shaded !Color
              | Cell !a

instance Automaton a => Automaton (Shaded a) where
  neighborhood (Shaded _) = []
  neighborhood (Cell a) = neighborhood a
  step f = shade (f $ V2 0 0) $ step $ f >=> deshade
    where deshade (Cell c) = Just c
          deshade _ = Nothing
          shade Nothing Nothing = Nothing
          shade _ (Just x) = Just $ Cell x
          shade (Just (Shaded c)) Nothing
            | c' == black = Nothing
            | otherwise = Just $ Shaded c'
            where c' = dark c
          shade (Just (Cell _)) Nothing = Just $ Shaded blue
  cellColor (Cell c) = cellColor c
  cellColor (Shaded c) = c
  defaultCell = Cell defaultCell

-- Graphics

data Game a = Game PointF Bool (World a)

draw :: Automaton a => PointF -> Game a -> Picture
draw (V2 x y) (Game o _ wld) = uncurry Scale (x, y) $ trans o $ Pictures pics
  where pics = map (\(c, a) -> Color (cellColor a) $ trans (liftA fromIntegral c) px) $ Map.toList wld
        px = Polygon [(-0.5, -0.5), (0.5, -0.5), (0.5, 0.5), (-0.5, 0.5)]
        trans (V2 x y) = Translate x y

events :: Automaton a => PointF -> Event -> Game a -> Game a
events _ (EventKey (SpecialKey k) Down _ _) (Game o@(V2 x y) s w)
  | k == KeyLeft = Game (V2 (x + 1) y) s w
  | k == KeyRight = Game (V2 (x - 1) y) s w
  | k == KeyDown = Game (V2 x (y + 1)) s w
  | k == KeyUp = Game (V2 x (y - 1)) s w
  | k == KeySpace = Game o (not s) w
events p (EventKey (MouseButton LeftButton) Down _ (uncurry V2 -> c)) (Game o s w) = Game o s $ Map.alter switch k w
  where k = liftA round $ (c / p - o)
        switch = maybe (Just defaultCell) (const Nothing)
events _ _ w = w

advance :: Automaton a => Float -> Game a -> Game a
advance _ (Game o p g) = Game o p $ if p then evaluate g else g

main :: IO ()
main = play (InWindow "Game of Life" size (0, 0)) bcolor fps (Game (V2 0 0) True game) (draw csize) (events csize) advance
  where size = (640, 480)
        csize = V2 8 8
        bcolor = black
        fps = 30
        game = Map.empty :: World (Shaded Conway's)
