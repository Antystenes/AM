{-# LANGUAGE TemplateHaskell
           , FlexibleInstances
           , MultiParamTypeClasses #-}
module Algmuz where
import Euterpea hiding (left, right)
import System.Random
import Control.Lens hiding (Iso, to, from)
import Data.List (foldl')
import Control.Comonad

ens = map pitch . drop 16 . take 49 $ ens'
  where ens' = 0:1:3:4:6:7:9:10: map (+12) ens'

wts = map pitch . drop 16 . take 49 $ wts'
  where wts' = 0:2:4:6:8:10: map (+12) wts'

data Line a = Line { _left   :: [a]
                   , _center ::  a
                   , _right  :: [a]} deriving Eq

makeLenses ''Line

instance Functor Line where
  fmap f (Line left x right) = Line (f <$> left) (f x) (f <$> right)

instance Foldable Line where
  foldr f acc (Line left x right) = foldl' (flip f) (f x $ foldr f acc right) left

instance Traversable Line where
  traverse f (Line up x down) = Line <$> traverse f up <*> f x <*> traverse f down

instance Show a => Show (Line a) where
  show = concatMap ((++" ").show)

data Grid a = Grid { _up    :: [Line a]
                   , _mid   ::  Line a
                   , _down  :: [Line a]} deriving Eq

makeLenses ''Grid

instance Functor Grid where
  fmap f (Grid a b c) = Grid ((fmap . fmap) f a) (fmap f b) ((fmap . fmap) f c)

instance Foldable Grid where
  foldr f acc (Grid up x down) = foldl' (foldr f) (foldr f (foldr (flip $ foldr f) acc down) x) up

instance Traversable Grid where
  traverse f (Grid up x down) = Grid <$> (traverse . traverse) f up <*> traverse f x <*> (traverse . traverse) f down

-- isomorphism between grid and line of lines

to (Grid up x down) = Line up x down

from (Line up x down) = Grid up x down

instance Show a => Show (Grid a) where
  show  = concatMap ((++"\n").show) . to

limap f = from . fmap f . to

moveLeftLine (Line (l:left) x right) = Line left l (x:right)
moveLeftLine x = x

moveRightLine (Line left x (r:right)) = Line (x:left) r right
moveRightLine x = x

moveLeftGrid :: Grid a -> Grid a
moveLeftGrid = limap moveLeftLine

moveRightGrid :: Grid a -> Grid a
moveRightGrid = limap moveRightLine

moveUpGrid = from . moveLeftLine . to

moveDownGrid = from . moveRightLine . to

appendToLine l w = over left (++[w]) l

height = length . concatMap pure . to

width = length . view mid

horizontalConfigurations :: Grid a -> Line (Grid a)
horizontalConfigurations g = Line leftC g rightC
  where
    leftC   = confs moveLeftGrid g . view (mid.left) $ g
    rightC  = confs moveRightGrid g . view (mid.right) $ g

confs f g = map ($ g) . tail . scanl (.) id . map (const f)

gridConfigurations :: Grid a -> Grid (Grid a)
gridConfigurations a@(Grid u _ d) = Grid up center down
  where
    center  = horizontalConfigurations a
    up      = map horizontalConfigurations $ confs moveUpGrid a u
    down    = map horizontalConfigurations $ confs moveDownGrid a d

instance Comonad Grid where
  extract    = view (mid.center)
  duplicate = gridConfigurations

lowerNeighb,upperNeighb,horizNeighb,mooresNeighb :: Grid a -> [a]
upperNeighb = toListOf $ up . ix 0 . (left . ix 0 <> center <> right . ix 0)
lowerNeighb = toListOf $ down . ix 0 . (left . ix 0 <> center <> right . ix 0)
horizNeighb = toListOf $ mid . (left . ix 0 <> right . ix 0)
mooresNeighb = upperNeighb <> lowerNeighb <> horizNeighb

counts :: Eq a => a -> [a] -> Int
counts = (length.).filter.(==)

data Cell = Alive | Dead deriving Eq

instance Show Cell where
  show Alive = "O"
  show Dead = " "

conwayUpdate :: Grid Cell -> Cell
conwayUpdate g =
  let aliveCounts = counts Alive . mooresNeighb $ g
  in case extract g of
    Dead  -> if aliveCounts == 3 then Alive else Dead
    Alive -> if aliveCounts > 3 || 2 > aliveCounts then Dead else Alive

folder b (Just (a,i), xs) =
  if a == b then (Just (a,i+1), xs)
  else (Just (b,1), \note -> note a (i+1) : xs note)
folder b (Nothing, xs) = (Just (b,1), xs)

postMerge (Nothing, xs)   = xs
postMerge (Just (a,b),xs) = \note -> note a b : xs note

getMelody p d a b = case a of
  Alive -> note (b/d) p
  Dead  -> rest (b/d)

foldLine :: (Foldable t0, Eq a) => t0 a -> (a -> Rational -> b) -> [b]
foldLine = postMerge . foldr folder (Nothing, const [])

getVoice d p = line . ($ getMelody p d)

getVoices :: [Pitch] -> Dur -> Grid Cell -> Music Pitch
getVoices s d =
  chord
  . map (uncurry $ getVoice d)
  . zip s
  . concatMap (pure . foldLine)
  . to

sampleLine = Line (concat . replicate 8 $ [Alive,Dead]) Dead (concat . replicate 7 $ [Alive,Dead])

bassLine = Line (concat . replicate 8 $ [Alive,Alive]) Alive (concat . replicate 7 $ [Alive,Alive])

bassLine2 = Line (concat . replicate 8 $ [Dead,Dead]) Dead (concat . replicate 7 $ [Dead,Dead])

sampleGrid = Grid (replicate 33 sampleLine ++ [bassLine, bassLine2, bassLine, bassLine2, bassLine]) sampleLine []

grids = iterate (=>> conwayUpdate) sampleGrid

greatMusic = playDevS 2 . line . take 100 . map (getVoices ens 8) $ grids

greatMusic2 n = playDevS 4 . line . take 100 . drop n . map (getVoices ens 8) $ grids

sampleGrid2 = Grid (replicate 10 sampleLine) sampleLine (replicate 10 sampleLine)

grids2 = iterate (\x -> x =>> conwayUpdate) sampleGrid2

greatMusic22 = playDev 4 . line . take 100 . map (getVoices wts 8) $ grids2

greatMusic23 = playDev 4 . line . take 100 . drop 10 . map (getVoices wts 8) $ grids2
