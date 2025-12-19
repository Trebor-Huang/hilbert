{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
module Main where
import Control.Monad
import Data.Array.Unboxed
import Data.Array.IArray
import Data.Time
import Control.Parallel
import Control.Parallel.Strategies
import System.IO
import GHC.IO
import Unsafe.Coerce
import Debug.Trace

-------- Hilbert Curve --------

type Cantor = [Bool] -- infinite list

hilbert :: Cantor -> (Cantor, Cantor)
-- Taken from http://bit-player.org/2013/mapping-the-hilbert-curve/
{-
hilbert (b1:b2:c) = let ~(x, y) = hilbert c in
  case (b1, b2) of
    (False, False) -> (False:y, False:x)
    (False, True) -> (False:x, True:y)
    (True, False) -> (True:x, True:y)
    (True, True) -> (True : map not y, False : map not x)
-}

-- We refactor it to avoid a sequence of `map not` building up
hilbert = _hilbert False

_hilbert :: Bool -> Cantor -> (Cantor, Cantor)
_hilbert b (b1 : b2 : c) = case (b1, b2) of
  (False, False) -> let ~(x, y) = _hilbert b c in (b : y, b : x)
  (False, True) -> let ~(x, y) = _hilbert b c in (b : x, not b : y)
  (True, False) -> let ~(x, y) = _hilbert b c in (not b : x, not b : y)
  (True, True) -> let ~(x, y) = _hilbert (not b) c in (not b : y, b : x)



-------- Seemingly Impossible Functional Program --------
-- find_vi taken from
--  https://math.andrej.com/2007/09/28/seemingly-impossible-functional-programs/
-- changing functions to infinite lists

find :: (Cantor -> Bool) -> Cantor
find p = b
 where
  b = map (\n -> not (q n (find (q n)))) [0..]
  q n a = p (insertAt n False b a)

forsome :: (Cantor -> Bool) -> Bool
forsome p = p (find p)

search :: (Cantor -> Bool) -> Maybe Cantor
search p = let c = find p in if p c then Just c else Nothing

insertAt :: Int -> a -> [a] -> [a] -> [a]
insertAt 0 z xs ys = z : ys
insertAt n z (x : xs) (y : ys) = x : insertAt (n-1) z xs ys



-------- Exact Real Arithmetic --------
-- Real arithmetic using signed digit streams
-- Taken from https://www.dcs.ed.ac.uk/home/mhe/plume/

data Signed = Z | P | N

i :: Signed -> Int
i P = 1
i Z = 0
i N = -1

ng :: Signed -> Signed
ng P = N
ng Z = Z
ng N = P

sig :: Int -> Signed
sig a = case compare a 0 of
  GT -> P
  EQ -> Z
  LT -> N

type Number = [Signed]

-- Average operator
infixl 6 ⊕
(⊕) :: Number -> Number -> Number
(⊕) x y = avg' x y 0

avg' :: Number -> Number -> Int -> Number
avg' (a0 : x) (b0 : y) c =
  if even d' then
    sig d' : avg' x y 0
  else avg'' x y d'
  where d' = i a0 + i b0 + 2*c

avg'' :: Number -> Number -> Int -> Number
avg'' (a1 : x') (b1 : y') d' = e : avg' (a1:x') (b1:y') c'
  where
    d = 2*d' + i a1 + i b1
    e | d > 2 = P
      | d < -2 = N
      | otherwise = Z
    c' = d' - (2 * i e)

mapping :: Cantor -> (Number, Number)
mapping c =
  let
    (u,v) = hilbert c
    x = fmap (unsafeCoerce.not) u
    y = unsafeCoerce v
    z = unsafeCoerce c  -- False maps to Z and True maps to P
  in
    (x ⊕ (ng <$> y), z ⊕ (ng <$> x ⊕ y))

-- near k x n:
--   outputs True if |n-x| < 2^(-k),
--   outputs False if |n-x| >= 2^(-k+1),
--   no guarantee otherwise
near :: (Ord a, Fractional a) => Int -> Number -> a -> Bool
near (-1) _ n = abs n < 3
near k _ n | abs n - 1 >= 3 = False

-- near 0 _ n = abs n < 1
-- near _ _ n | abs n >= 1 = False

near k (P:x) n = near (k-1) x (n*2-1)
near k (Z:x) n = near (k-1) x (n*2)
near k (N:x) n = near (k-1) x (n*2+1)

main :: IO ()
main = finalResult `seq` withFile "./htest.pgm" WriteMode \handle -> do
  hPutStrLn handle "P2"
  hPutStrLn handle (show (2*n+1) ++ " " ++ show (2*m+1))
  hPutStrLn handle "255"

  forM_ [-m..m] \iy -> do
    forM_ [-n..n] \ix -> do
      hPutStr handle (show (finalResult ! (ix, iy)) ++ " ")
    hPutStrLn handle ""

-- Resolution parameter, note that it's exponential and p=10 is about the limit
p = 6 :: Int
q = 6 :: Int

n = 2^(p-1) :: Int
m = 2^(q-1) :: Int

coord :: (Int, Int) -> (Rational, Rational)
coord (i, j) =
  (fromIntegral i / fromIntegral (n*2), fromIntegral j / fromIntegral (m*2))

toColor :: Cantor -> Int
-- simple coloring scheme
toColor x = 100 + sum
  (zipWith (\b i -> if b then 2^i else 0) (drop 2 x) [6,5,4,3,2,1,0])

computation (coord -> (x', y')) =
  case search \c ->
    let (x,y) = mapping c in near (p+1) x x' && near (q+1) y y' of
    Just c -> toColor c
    Nothing -> if abs y' <= 1/2 - abs x' / 2 then 0 else 60

-- Unsafe way to give a rough progress indication
-- It's gonna be slightly out of order but who cares
withProgress (ix, iy) | iy == m = trace
  (show (ix+n) ++ "/" ++ show (2*n+1))
  (computation (ix, iy))
withProgress p = computation p

finalResult :: UArray (Int, Int) Int
finalResult = unsafePerformIO do
  t <- getCurrentTime
  let !result = listArray ((-n,-m), (n,m))
        (map withProgress (range ((-n, -m), (n, m)))
          `using` parListChunk 64 rdeepseq)
  t' <- getCurrentTime
  print (diffUTCTime t' t)
  return result
{-# NOINLINE finalResult #-}
