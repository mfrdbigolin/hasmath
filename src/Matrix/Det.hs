{- Copyright (C) 2021 Matheus Fernandes Bigolin <mfrdrbigolin@disroot.org>
 - SPDX-License-Identifier: MIT
 -}

{-|
 - Description: Calculate the determinant of a matrix
 - Date: 2020/05/26
 -}

module Det (det, cof, minor) where


-- |Calculate the determinant of a square matrix.
det :: (Num a) => [[a]] -> a
-- |Calculate the cofactor of a square matrix.
cof :: (Num a) => [[a]] -> Int -> Int -> a
-- |Find a minor square matrix by removing the i-th row and j-th column.
minor :: [[a]] -> Int -> Int -> [[a]]

det [[a]] = a
det as
  | error_matrix as "det"
  = sum [(as!!i!!0) * (cof as (i + 1) 1) | i <- [0..length as - 1]]

cof as i j
  | (error_matrix as "cof") && (error_bound as i j "cof")
  = (if even (i + j) then 1 else -1) * det (minor as i j)

minor as i j
  | (error_matrix as "minor") && (error_bound as i j "minor")
  = [[[x!!k | x <- as]!!l | k <- [0..length as - 1], k /= (j - 1)]
                          | l <- [0..length as - 1], l /= (i - 1)]


-- |Abort if the list is null or the matrix is non-square.
error_matrix :: [[a]] -> String -> Bool
-- |Abort if the given indices of a matrix are out of bound.
error_bound :: [[a]] -> Int -> Int -> String -> Bool
-- |Check if a given matrix is square.
is_square :: [[a]] -> Bool

error_matrix as func
  | null as || null (as!!0)
  = error ("[" ++ func ++ "] empty list")
  | not (is_square as) = error ("[" ++ func ++ "] non-square matrix")
  | otherwise = True

error_bound as i j func
  | (i > length as) || (any (\a -> (length a) < j) as)
  = error ("[" ++ func ++ "] index out of range")
  | otherwise = True

is_square as = all (== (length as)) (map (length) as)
