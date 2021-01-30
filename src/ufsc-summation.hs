{- Copyright (C) 2021 Matheus Fernandes Bigolin <mfrdrbigolin@disroot.org>
 - SPDX-License-Identifier: MIT
 -}

{-|
 - Description: Use the UFSC entrance exam method to calculate the number
   of points for a question with a summation multiple-choice format.
 - Date: 2020/07/03

 - Punctuation (P) calculation method (in pseudo-Haskell):
   P = if NPC > NPI then
         (NP – NTPC – NPC – NPI)/NP
       else
         0.00

       where NP = Number of alternatives in the question
           NTPC = Total number of correct alternatives
            NPC = Number of correct alternatives marked by the candidate
            NPI = Number of wrong alternatives marked by the candidate

 - Source: https://web.archive.org/web/20200225133224/http://www.vestibular2009.ufsc.br/guia_vestibulando/questoes.html (in Portuguese)
 -}

module Ufsc (summation) where


import Data.Bits

-- |Summation with 'n' alternatives, being 'c' the correct sum and 's'
-- the candidate's sum.
summation :: (RealFrac a) => Int -> Int -> Int -> a
summation n c s
  | error_num n "total" && error_range n c "correct"
    && error_range n s "candidate"
  = let sum_correct = count (c .&. s)
        sum_wrong = count ((127 - c) .&. s)
    in if (sum_correct > sum_wrong) then
         round_frac (1 + fromIntegral (sum_correct - sum_wrong - count c)
                     / (fromIntegral n))
       else
         0

-- |Round 'n' to the nearest hundredth.
round_frac :: (RealFrac a) => a -> a
round_frac n = fromIntegral (round (n * 100) :: Integer) / 100

-- |Err if the number of alternatives, 'n', is not in the range [1, 7].
error_num :: Int -> String -> Bool
error_num n ident
  | not (n `elem` [1..7])
  = error ("[" ++ ident ++ "] invalid number of affirmatives")
  | otherwise = True

-- |Err if the summation 's' is inconsistent with the number of
-- alternatives, 'n', or greater than 99.
error_range :: Int -> Int -> String -> Bool
error_range n s ident
  | not (s `elem` [1..99]) || s > (2 ^ n - 1)
  = error ("[" ++ ident ++ "] invalid summation")
  | otherwise = True

-- |Haskell's 'until', but returns the number of iterations instead of
-- the final result.
until_it :: (a -> Bool) -> (a -> a) -> a -> Int
until_it b f a = until_it' b f a 0

until_it' :: (a -> Bool) -> (a -> a) -> a -> Int -> Int
until_it' b _ a acc | b a = acc
until_it' b f a acc | otherwise = until_it' (b) (f) (f a) (acc + 1)

-- |Count the number of one-bits in the binary representation of 'n'.
count :: Int -> Int
count n = until_it (== 0) (\x -> x .&. (x - 1)) n
