{- Copyright (C) 2021 Matheus Fernandes Bigolin <mfrdrbigolin@disroot.org>
 - SPDX-License-Identifier: MIT
 -}

{-|
 - This module is INCOMPLETE.
 -
 - Description: Pascal triangle
 - Date: 2020/08/18
 -}

module Pascal (mirror{-, pascal-}) where


-- |Mirror an array with itself.
mirror :: [a] -> [a]
mirror as | length as `rem` 2 == 0 = as ++ reverse as
          | otherwise = as ++ (drop 1 (reverse as))


-- INCOMPLETE functions infra.

--line' :: Int -> Int -> [Int]
--line' n k = 

--line :: Int -> [Int]
--line n = mirror (line' n 1)

--pascal :: Int -> [[Int]]
--pascal n = [line k | k <- [0..n - 1]]
