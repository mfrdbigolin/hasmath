{- Copyright (C) 2021 Matheus Fernandes Bigolin <mfrdrbigolin@disroot.org>
 - SPDX-License-Identifier: MIT
 -}

{-|
 - Description: Analyis of the 4th problem of Putnam 2018
 - Date: 2020/06/06
 -}

module Putnam where


x :: (Num a) => a -> Integer -> a
x _ 0 = 1
x a n | n == 1 || n == 2 = a
x a n = 2 * x a (n - 1) * x a (n - 2) - x a (n - 3)

-- |'n'th element of the Fibonacci sequence.
fib :: (Num a) => Integer -> a
fib n | n == 1 || n == 2 = 1
fib n = fib (n - 1) + fib (n - 2)
