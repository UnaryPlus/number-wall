{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
module Main (main) where

import NumberWall
import Data.Foldable (foldlM)
import Control.Monad (when, unless)
import System.Exit (exitFailure)

data Test = Test String Bool

runTests :: [Test] -> IO ()
runTests tests = do
  exit <- foldlM
    (\exit (Test name pass) ->
      (not pass || exit) <$
      unless pass (putStrLn ("Test failed: " ++ name)))
    False tests
  when exit exitFailure

match2 :: a -> a -> Mod 2 -> a
match2 x0 x1 n =
  case unMod n of
    0 -> x0
    _ -> x1

match3 :: a -> a -> a -> Mod 3 -> a
match3 x0 x1 x2 n =
  case unMod n of
    0 -> x0
    1 -> x1
    _ -> x2

pagoda0 = numberWall (pagoda :: Int -> Integer)
pagoda2 = numberWall (pagoda :: Int -> Mod 2)
pagoda3 = numberWall (pagoda :: Int -> Mod 3)
pagoda5 = numberWall (pagoda :: Int -> Mod 5)

square0 = numberWall ((^2) . fromIntegral :: Int -> Integer)
square2 = numberWall ((^2) . fromIntegral :: Int -> Mod 2)
square3 = numberWall ((^2) . fromIntegral :: Int -> Mod 3)

cube0 = numberWall ((^3) . fromIntegral :: Int -> Integer)

window n
  | n == (-2) || n == (-1) = 2
  | n >= 0 && n < 6 = 2 ^ n
  | otherwise = 1

window0 = numberWall (window :: Int -> Integer)
rueppel2 = numberWall (rueppel :: Int -> Mod 2)
ternary3 = numberWall (ternary :: Int -> Mod 3)

main :: IO ()
main = do
  runTests testCases

  saveImage "test/pagoda.png"
    (match2 (181, 118, 46) (0, 0, 0))
    (0, 224) (0, 124) pagoda2

  saveImage "test/rueppel.png"
    (match2 (255, 255, 255) (0, 0, 0))
    (0, 128) (-1, 128) rueppel2

  saveImage "test/ternary.png"
    (match3 (0, 0, 0) (183, 24, 53) (207, 181, 84))
    (-72, 72) (-1, 72) ternary3

  saveImage "test/pagoda-mod3.png"
    (match3 (255, 255, 255) (0, 0, 0) (53, 139, 142))
    (-50, 50) (50, 150) pagoda3

testCases :: [Test]
testCases =
  [ Test "pagoda sequence" $
      showSection show (0, 9) (-1, 7) pagoda0 ==
      " 1  1  1  1  1  1  1  1  1 \n\
      \-1  0  1  0 -1  1  1 -1 -1 \n\
      \ 1  1  1  1  1  2  2  2  1 \n\
      \ 1 -1  0  1  1  2  0 -2  1 \n\
      \-2  1  1  1 -1  2  2  2  1 \n\
      \-1 -3  1  2 -1  3  1 -1 -1 \n\
      \13 10  7  5  5  5  2  1  2 \n\
      \ 1 -3 -1 -5  0  5 -1  3  1 \n"

  , Test "pagoda sequence (single value)" $
      pagoda0 7 2 == (-2)

  , Test "pagoda sequence mod 2" $
      showSection (match2 "0" ".") (0, 9) (-1, 7) pagoda2 ==
      ". . . . . . . . . \n\
      \. 0 . 0 . . . . . \n\
      \. . . . . 0 0 0 . \n\
      \. . 0 . . 0 0 0 . \n\
      \0 . . . . 0 0 0 . \n\
      \. . . 0 . . . . . \n\
      \. 0 . . . . 0 . 0 \n\
      \. . . . 0 . . . . \n"

  , Test "pagoda sequence mod 2 (single value)" $
      pagoda2 6 4 == 1

  , Test "pagoda sequence mod 3" $
      showSection (show . unMod) (0, 9) (-1, 7) pagoda3 ==
      "1 1 1 1 1 1 1 1 1 \n\
      \2 0 1 0 2 1 1 2 2 \n\
      \1 1 1 1 1 2 2 2 1 \n\
      \1 2 0 1 1 2 0 1 1 \n\
      \1 1 1 1 2 2 2 2 1 \n\
      \2 0 1 2 2 0 1 2 2 \n\
      \1 1 1 2 2 2 2 1 2 \n\
      \1 0 2 1 0 2 2 0 1 \n"

  , Test "pagoda sequence mod 5" $
      showSection (show . unMod) (0, 9) (-1, 7) pagoda5 ==
      "1 1 1 1 1 1 1 1 1 \n\
      \4 0 1 0 4 1 1 4 4 \n\
      \1 1 1 1 1 2 2 2 1 \n\
      \1 4 0 1 1 2 0 3 1 \n\
      \3 1 1 1 4 2 2 2 1 \n\
      \4 2 1 2 4 3 1 4 4 \n\
      \3 0 2 0 0 0 2 1 2 \n\
      \1 2 4 0 0 0 4 3 1 \n"

  , Test "squares" $
      showSection show (-1, 9) (0, 5) square0 ==
      "  1   0   1   4   9  16  25  36  49  64 \n\
      \  1  -1   1   7  17  31  49  71  97 127 \n\
      \  8   8   8   8   8   8   8   8   8   8 \n\
      \  0   0   0   0   0   0   0   0   0   0 \n\
      \  0   0   0   0   0   0   0   0   0   0 \n"

  , Test "squares (single value)" $
      square0 100 2 == 8

  , Test "squares mod 2" $
      showSection (show . unMod) (-1, 9) (0, 5) square2 ==
      "1 0 1 0 1 0 1 0 1 0 \n\
      \1 1 1 1 1 1 1 1 1 1 \n\
      \0 0 0 0 0 0 0 0 0 0 \n\
      \0 0 0 0 0 0 0 0 0 0 \n\
      \0 0 0 0 0 0 0 0 0 0 \n"

  , Test "squares mod 3" $
      showSection (show . unMod) (-1, 9) (0, 5) square3 ==
      "1 0 1 1 0 1 1 0 1 1 \n\
      \1 2 1 1 2 1 1 2 1 1 \n\
      \2 2 2 2 2 2 2 2 2 2 \n\
      \0 0 0 0 0 0 0 0 0 0 \n\
      \0 0 0 0 0 0 0 0 0 0 \n"

  , Test "cubes" $
      showSection show (1, 6) (1, 5) cube0 ==
      "   1   37  217  721 1801 \n\
      \ -36  144  756 2016 4140 \n\
      \1296 1296 1296 1296 1296 \n\
      \   0    0    0    0    0 \n"

  , Test "4x4 window" $
      showSection show (-1, 7) (0, 7) window0 ==
      "          2           1           2           4           8          16          32           1 \n\
      \          2          -3           0           0           0           0        1008         -31 \n\
      \          5           9           0           0           0           0       31752         961 \n\
      \         -1         -27           0           0           0           0     1000188      -29791 \n\
      \         11          81           0           0           0           0    31505922      923521 \n\
      \        -40        -243        5103     -107163     2250423   -47258883   992436543   -28629151 \n\
      \         35        3249       13284     1520775    25498017   250187058 31218807378   887503681 \n"
  ]
