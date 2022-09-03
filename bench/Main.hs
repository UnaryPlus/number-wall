{-# LANGUAGE DataKinds #-}

module Main where

import NumberWall
import Test.Tasty.Bench

showPagoda n =
  showSection show (0, n * 2) (0, n) $ numberWall ((pagoda :: Int -> Mod 2) . (+ n))

main = defaultMain
  [ bench "showPagoda" $ nf showPagoda 256
  ]
