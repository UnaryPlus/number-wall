{-|
Module:    NumberWall
Copyright: (c) Owen Bechtel, 2022
License:   MIT

Example usage:

@
wall = numberWall (pagoda :: Int -> Mod 2)
color x = case unMod x of
  0 -> (181, 118, 46)
  1 -> (0, 0, 0)
saveImage "pagoda.png" color (0, 256) (0, 128) wall
@
-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NumberWall
  ( -- * Creating number walls
    Col, Row, numberWall, NumberWall
    -- * Special sequences
  , pagoda, rueppel, ternary
    -- * Displaying number walls
  , saveImage, showSection, printSection
    -- * Modular arithmetic
  , module Data.Mod.Word
  ) where

import Prelude hiding (negate, (*), (+), (-), (^), quot)

import Data.Chimera (VChimera, index, tabulateFix')
import Data.Chimera.ContinuousMapping (fromZCurve, toZCurve, wordToInt, intToWord)
import Data.Semiring (Semiring, Ring, zero, one, negate, (*), (+), (-), (^))
import Data.Euclidean (Euclidean, quot)

import Codec.Picture (PixelRGB8(..), generateImage, writePng)
import Data.Mod.Word
import Data.Word (Word8)

{-|
The 'numberWall' function works for any Euclidean domain. (In other words,
there must be some sort of @div@ function, along with addition and multiplication).
Usually, this domain is either 'Integer' or @Mod p@ for some prime number p.
Although 'Int' and @Mod n@ for non-prime n also have 'Euclidean' instances, they
are not actually Euclidean domains, and using 'numberWall' with them often causes
divide-by-zero errors.
-}
type NumberWall a = (Eq a, Ring a, Euclidean a)

type Col = Int
type Row = Int

sign :: Ring a => Int -> a
sign x = if even x then one else negate one

memoFix2 :: forall a. ((Int -> Int -> a) -> (Int -> Int -> a)) -> (Int -> Int -> a)
memoFix2 f = uncast $ index (tabulateFix' (cast . f . uncast) :: VChimera a)
  where
    cast :: (Int -> Int -> a) -> (Word -> a)
    cast f = \n -> let (x, y) = fromZCurve n in
     f (wordToInt x) (wordToInt y)

    uncast :: (Word -> a) -> (Int -> Int -> a)
    uncast g = \x y -> g (toZCurve (intToWord x) (intToWord y))

{-|
Generate the number wall for a sequence.
-}
numberWall :: NumberWall a => (Int -> a) -> Col -> Row -> a
numberWall s = memoFix2 \recurse col row ->
  let f a b = recurse (col + a) (row - b) in
  case row of
    --simple cases
    _ | row < (-1) -> zero
    -1 -> one
    0 -> s col

    --small cross rule
    _ | f 0 2 /= zero ->
        (f 0 1 ^ 2 - f (-1) 1 * f 1 1) `quot` f 0 2

    --large cross rule
      | f 0 3 /= zero ->
        (f 2 2 * f (-1) 2 ^ 2 + f (-2) 2 * f 1 2 ^ 2 - f 0 4 * f 0 1 ^ 2) `quot` f 0 3 ^ 2

    --two rows below window
      | f 0 1 /= zero ->
        let top = findTop f (0, 4)
            size = top - 2
            right = findRight f (1, top - 1)
            left = right - size - 1
            k = right

            _A = f (left + k) top
            _B = f left (top - k)
            _C = f right (1 + k)
            _D = f 0 1

            _E = f (left + k) (top + 1)
            _F = f (left - 1) (top - k)
            _G = f (right + 1) (1 + k)

            _P = f (left + k - 1) top
            _Q = f left (top - k + 1)
            _R = f right (2 + k)
            _T = f (-1) 1
        in
        (_P * _B * _B * _C * _D * _E
        + sign k * _Q * _A * _A * _C * _D * _F
        - sign k * _T * _P * _Q * _A * _B * _G)
        `quot` (_R * _P * _Q * _A * _B)

      | otherwise ->
        let top = findTop f (0, 4)
            size = top - 1
        in
        case searchRight f size (1, top - 1) of
          --inside window
          Nothing -> zero
          Just right
            | f (right - size - 1) (top - 1) == zero -> zero

          --one row below window
            | otherwise ->
              let left = right - size - 1
                  k = right
              in
              sign (size * k) * f left (top - k) * f right k `quot` f (left + k) top

findTop :: NumberWall a => (Col -> Row -> a) -> (Col, Row) -> Row
findTop f (col, row)
  | f col row == zero = findTop f (col, row + 1)
  | otherwise = row

findRight :: NumberWall a => (Col -> Row -> a) -> (Col, Row) -> Col
findRight f (col, row)
  | f col row == zero = findRight f (col + 1, row)
  | otherwise = col

searchRight :: NumberWall a => (Col -> Row -> a) -> Int -> (Col, Row) -> Maybe Col
searchRight f limit (col, row)
  | limit <= 0 = Nothing
  | f col row == zero = searchRight f (limit - 1) (col + 1, row)
  | otherwise = Just col

{-|
The pagoda sequence (&#8203;[A301849](https://oeis.org/A301849)).
In mod 2, its number wall is a self-similar fractal.
In mod 3 and mod 7, all zeros in its number wall are isolated.
-}
pagoda :: Ring a => Int -> a
pagoda n = bit (n + 1) - bit (n - 1)
  where
    bit k
      | k == 0 = zero
      | even k = bit (k `div` 2)
      | k `mod` 4 == 1 = zero
      | otherwise = one

{-|
The Fredholm-Rueppel sequence (&#8203;[A036987](https://oeis.org/A036987)).
@rueppel n@ evaluates to 1 if n + 1 is a power of 2, and 0 otherwise.
Its number wall contains zero-windows of exponentially increasing size, and
an infinite diagonal line of ones.
-}
rueppel :: Semiring a => Int -> a
rueppel n
  | n < 0 = zero
  | otherwise =
    let pow = logBase 2 (fromIntegral (n + 1))
    in if ceiling pow == floor pow then one else zero

data Alpha = A | B | C | D | E | F

{-|
(&#8203;[A039974](https://oeis.org/A039974)). The mod-3 number wall of this sequence
has an infinite central region with no zeros.
-}
ternary :: Ring a => Int -> a
ternary n
  | n < 0 = negate (ternary (-n - 1))
  | otherwise =
    case ternary' n of
      A -> one
      B -> zero
      C -> one
      D -> zero
      E -> negate one
      F -> negate one
    where
      ternary' 0 = A
      ternary' x =
        let (q, m) = x `divMod` 3
            match3 a b c =
              case m of
                0 -> a
                1 -> b
                _ -> c
        in case ternary' q of
          A -> match3 A C B
          B -> match3 B C B
          C -> match3 E D F
          D -> match3 D D D
          E -> match3 E D D
          F -> match3 D D F

{-|
RGB colors.
-}
type Color = (Word8, Word8, Word8)

{-|
Save a number wall as a PNG file.
-}
saveImage
  :: FilePath          -- ^ File name
  -> (a -> Color)      -- ^ Function assigning each number a color
  -> (Col, Col)        -- ^ Column range
  -> (Row, Row)        -- ^ Row range
  -> (Col -> Row -> a) -- ^ Number wall
  -> IO ()
saveImage path toColor (minC, maxC) (minR, maxR) wall =
  let convert (r, g, b) = PixelRGB8 r g b
      image = generateImage
        (\a b -> convert $ toColor $ wall (minC + a) (minR + b))
        (maxC - minC) (maxR - minR)
  in writePng path image

loop = flip map

maxOf measure = foldr (max . measure) minBound

{-|
Convert a section of a number wall into a string.
-}
showSection :: (a -> String) -> (Col, Col) -> (Row, Row) -> (Col -> Row -> a) -> String
showSection toString (minC, maxC) (minR, maxR) wall =
  let chunks = loop [minR..maxR-1] \r -> loop [minC..maxC-1] \c -> toString (wall c r)
      len = maxOf (maxOf length) chunks
      pad s = replicate (len - length s) ' ' ++ s ++ " "
  in concatMap (\xs -> concatMap pad xs ++ "\n") chunks

{-|
Print a section of a number wall.
-}
printSection :: (a -> String) -> (Col, Col) -> (Row, Row) -> (Col -> Row -> a) -> IO ()
printSection toString cols rows wall = putStr (showSection toString cols rows wall)
