{- Copyright (C) 2021 Matheus Fernandes Bigolin <mfrdrbigolin@disroot.org>
 - SPDX-License-Identifier: MIT
 -}

{-|
 - Description: Apply the Newton-Raphson method to find the roots of a
   polynomial function
 - Date: 2020/08/19 and 2021/01/30 (conclusion)
 -}

module Newton (functionalize, derive, newton) where


-- |Transform an array of coefficients 'cs' into a polynomial function with
-- argument 'y'.
functionalize :: (Num a) => [a] -> a -> a
functionalize cs = \y -> sum [fs y | fs <- [\x -> a*x^(length cs - i - 1)
                                           | (i,a) <- zip [0..] cs]]

-- |Derive a polynomial function ('cs'!!0)*x^n + .. + ('cs'!!n)*x^0 = 0,
-- where the elements of 'cs' are the function's coefficients.
derive :: (Num a) => [a] -> [a]
derive cs = [c!!(length c - i - 1) * (fromIntegral (i+1))
            | i <- reverse [0..length c - 1]]
  where c = init cs

-- |Newtonian method with 'n' iterations, initial guess 'x0' and
-- coefficients 'cs'.
newton :: (Num a, Fractional a) => Int -> a -> [a] -> a
newton 0 x0 _ = x0
newton n x0 cs = newton (n - 1) (x0 - (f x0) / (f' x0)) cs
  where f = functionalize cs
        f' = functionalize (derive cs)
