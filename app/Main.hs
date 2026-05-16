{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where
import Control.Monad
import Data.Bits
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

data Stream a = a :! Stream a deriving (Functor)
type Cantor = Stream Bool

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
_hilbert b (b1 :! (b2 :! c)) = case (b1, b2) of
  (False, False) -> let ~(x, y) = _hilbert b c in (b :! y, b :! x)
  (False, True) -> let ~(x, y) = _hilbert b c in (b :! x, not b :! y)
  (True, False) -> let ~(x, y) = _hilbert b c in (not b :! x, not b :! y)
  (True, True) -> let ~(x, y) = _hilbert (not b) c in (not b :! y, b :! x)



-------- Seemingly Impossible Functional Program --------
-- find_vi taken from
--  https://math.andrej.com/2007/09/28/seemingly-impossible-functional-programs/
-- changing functions to infinite lists

find :: (Cantor -> Bool) -> Cantor
find p = b
 where
  b = go 0
  go n = not (q n (find (q n))) :! go (n + 1)
  q n a = p (insertAt n False b a)

forsome :: (Cantor -> Bool) -> Bool
forsome p = p (find p)

search :: (Cantor -> Bool) -> Maybe Cantor
search p = let c = find p in if p c then Just c else Nothing

insertAt :: Int -> Bool -> Cantor -> Cantor -> Cantor
insertAt 0 z xs ys = z :! ys
insertAt n z (x :! xs) (y :! ys) = x :! insertAt (n-1) z xs ys



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

type Number = Stream Signed

-- Average operator
infixl 6 ⊕
(⊕) :: Number -> Number -> Number
(⊕) x y = avg' x y 0

avg' :: Number -> Number -> Int -> Number
avg' (a0 :! x) (b0 :! y) c =
  if even d' then
    sig d' :! avg' x y 0
  else avg'' x y d'
  where d' = i a0 + i b0 + 2*c

avg'' :: Number -> Number -> Int -> Number
avg'' x0@(a1 :! x') y0@(b1 :! y') d' = e :! avg' x0 y0 c'
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
    z = unsafeCoerce c
  in
    (x ⊕ (ng <$> y), z ⊕ (ng <$> (x ⊕ y)))

data Dyadic = !Int :/^ !Int
incr :: Dyadic -> Dyadic
incr (n :/^ e) = (n + shiftL 1 e) :/^ e
decr :: Dyadic -> Dyadic
decr (n :/^ e) = (n - shiftL 1 e) :/^ e
double :: Dyadic -> Dyadic
double (n :/^ 0) = (shiftL n 1) :/^ 0
double (n :/^ e) = n :/^ (e - 1)

exp2 :: Int -> Int
exp2 n | n < 0 = 1
exp2 n = shiftL 1 n

-- near k x n:
--   outputs True if |n-x| < 2^(-k),
--   outputs False if |n-x| >= 2^(-k+1),
-- no guarantee otherwise
near :: Int -> Number -> Dyadic -> Bool
-- Since Number lies between +1 and -1
-- If |n| >= 1 + 2^(-k+1) then we're done and return false
-- If k = 0 and |n| >= 3 in particular, then we output False for sure
-- If k = 0 and n = 0, then we can output True for sure
-- If k = 0 and |n| < 2, then it is possible that we have to return True
-- If k = 0 and |n| >= 1, then it is possible that we have to return False
-- So we can't stop at k = 0
-- If k = -1 and |n| < 1, then we can output True for sure
-- If k = -1 and |n| < 3, then it is possible that we have to return True
-- If k = -1 and |n| >= 3, then it is possible that we have to return False
near (-1) _ (n :/^ e) = abs n < shiftL 3 e
near k _ (n :/^ e)
  | abs n >= shiftL 1 e + exp2 (e-k+1)
  = False

near k (P:!x) n = near (k-1) x (decr (double n))
near k (Z:!x) n = near (k-1) x (double n)
near k (N:!x) n = near (k-1) x (incr (double n))

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

coord :: (Int, Int) -> (Dyadic, Dyadic)
coord (i, j) = (i :/^ p, j :/^ q)  -- divides [-1/2, 1/2] into 2^p pieces
{-# INLINE coord #-}

toColor :: Cantor -> Int
-- simple coloring scheme
toColor x = 100 + go 8 x
  where
    go n x | n < 0 = 0
    go n (_ :! x) | n > 6 = go (n-1) x
    go n (True :! x) = go (n-1) x + shiftL 1 n
    go n (False :! x) = go (n-1) x


computation (coord -> (x', y')) =
  case search \c ->
    -- since the pixel distance is 2^(-p),
    -- we need 2^(-p-1) amount of guaranteed space
    let (x,y) = mapping c in near (p+1) x x' && near (q+1) y y' of
    Just c -> toColor c
    Nothing -> 0

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
