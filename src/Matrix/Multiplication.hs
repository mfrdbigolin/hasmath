{- Copyright (C) 2021 Matheus Fernandes Bigolin <mfrdrbigolin@disroot.org>
 - SPDX-License-Identifier: MIT
 -}

{-|
 - This module is PARTIALLY INCOMPLETE.
 -
 - Description: Calculate the matrix multiplication
 - Date: 2020/06/09
 -}

module Multiplication (sub_col, equation) where


import Det (det)

sub_col :: [[a]] -> [a] -> Int -> [[a]]
sub_col as bs j
  = [[[if (k == (j - 1)) then bs!!l else x!!k | x <- as]!!l
                                              | k <- [0..length as - 1]]
                                              | l <- [0..length as - 1]]

equation :: (Num a, Fractional a) => [[a]] -> [a] -> [a]
equation as bs = [det (sub_col as bs i) / det as
                 | i <- [1..length as]]

