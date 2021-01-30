{- Copyright (C) 2021 Matheus Fernandes Bigolin <mfrdrbigolin@disroot.org>
 - SPDX-License-Identifier: MIT
 -}

{-|
 - This module is INCOMPLETE.
 -
 - Description: Simplify fractions to an irreducible format
 - Date: 2020/06/13
 -}

module Simplifier (dec{-, simpl-}) where


import Data.List

-- |Separate a number 'n' into strings containing the integer and the
-- fractional part of this number.
dec :: (Show a, Num a) => a -> (String, String)
dec n = case (elemIndex '.' (show n)) of
  Just k -> (fst exs, tail (snd exs)) where exs = splitAt k (show n)
  Nothing -> error "IMPOSSIBLE!"

-- INCOMPLETE function.
--simpl :: (Num a) => (a, a) -> (a, a)
--simpl frc = d
