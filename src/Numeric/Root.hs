{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Numeric.Root where
import Data.Maybe

data Interval a = Interval { lowerBound :: !a
                           , upperBound :: !a
                           } deriving (Read, Show, Eq, Ord)

-- | Expand the given interval to bracket some root
zbrac :: (Ord a, Eq a, Num a)
      => a                      -- ^ scale factor
      -> (a -> a)               -- ^ function to bracket root
      -> Interval a             -- ^ initial non-empty interval
      -> Maybe (Interval a)     -- ^ expanded interval bracketing root
zbrac factor f int0
  | lowerBound int0 == upperBound int0 = Nothing
  | otherwise = loop (0 :: Int) int0
  where
    loop !n int@(Interval lb ub)
      | n >= maxTry = Nothing
      | otherwise   =
        let !f1 = f lb; !f2 = f ub
        in if f1 * f2 < 0
           then Just int
           else if abs f1 < abs f2
                then loop (n+1) (Interval (lb + factor * (lb - ub)) ub)
                else loop (n+1) (Interval lb (ub + factor * (ub - lb)))
    maxTry = 50

-- | Subdivide the given interval and find intervals bracketing roots
zbrak :: (Fractional a, Ord a, Eq a, Num a)
      => (a -> a)               -- ^ function to bracket roots
      -> Int                    -- ^ division count
      -> Interval a             -- ^ initial non-empty interval to divide
      -> [Interval a]           -- ^ intervals bracketing a root
zbrak f n int0 = mapMaybe step [0.. n - 1]
  where
    !dx = (upperBound int0 - lowerBound int0) / fromIntegral n
    step k =
      let x  = lowerBound int0 + dx * fromIntegral k
          x' = x + dx
      in if f x * f x' <= 0
         then Just $ Interval x x'
         else Nothing

-- | Find the root (or singularity) in the given interval by bisection method.
bisect :: (Fractional a, Ord a, Eq a, Num a)
       => Int                   -- ^ maximum iteration count
       -> a                     -- ^ precision
       -> (a -> a)              -- ^ function to find the root
       -> Interval a            -- ^ an interval bracketing a root
       -> Maybe a               -- ^ root
bisect try prec f (Interval x1 x2)
  |  f x1 * f x2 >= 0 = Nothing
  | otherwise =
    let (dx0, rtb0) = if f x1 < 0
                      then (x2 - x1, x1)
                      else (x1 - x2, x2)
        loop !n !dx !rtb
          | n > try   = Nothing
          | otherwise =
            let dx'   = dx * 0.5
                xmid = rtb + dx'
                fmid  = f xmid
                rtb'  = if fmid <= 0
                        then xmid
                        else rtb
            in if abs dx' < prec || fmid == 0
               then Just rtb'
               else loop (n+1) dx' rtb'
    in loop (0 :: Int) dx0 rtb0

falsePosition :: (Fractional a, Ord a, Eq a, Num a)
              => Int            -- ^ max iteration
              -> a              -- ^ precision
              -> (a -> a)       -- ^ function to find roots
              -> Interval a     -- ^ bracketing interval
              -> Maybe a        -- ^ root in the given interval
falsePosition its prec fun (Interval x1 x2) =
  let (xl0, xh0)
        | fun x1 < 0  = (x1, x2)
        | otherwise = (x2, x1)
      loop !n !xl !xh
        | n >= its = Nothing
        | otherwise =
          let !fl = fun xl; !fh = fun xh
              !dx = xl - xh
              !rtf = xl + dx * fl / (fl - fh)
              !f = fun rtf
              (!del, xl', xh') =
                if f < 0
                then (xl-rtf, rtf, xh)
                else (xh-rtf, xl, rtf)
          in if abs del < prec || f == 0
             then Just rtf
             else loop (n+1) xl' xh'
  in loop 0 xl0 xh0

secant :: (Fractional a, Ord a, Eq a, Num a)
          => Int -> a -> (a -> a) -> Interval a -> Maybe a
secant its prec fun (Interval x1 x2) =
  let (rts0, xl0, fl0, f0)
        | abs (fun x1) < abs (fun x2) = (x1, x2, fun x2, fun x1)
        | otherwise = (x2, x1, fun x1, fun x2)
      loop !n !rts !xl !fl !f
        | n >= its  = Nothing
        | otherwise =
          let dx = (xl - rts)*f/(f-fl)
              xl' = rts
              fl' = f
              rts' = rts + dx
              f' = fun rts'
          in if abs dx < prec || f == 0
             then Just rts'
             else loop (n+1) rts' xl' fl' f'
  in loop 0 rts0 xl0 fl0 f0
