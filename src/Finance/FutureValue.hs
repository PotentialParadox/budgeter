module Finance.FutureValue
  (futureValue) where

futureValue :: Integer -> Double -> Double -> Double -> Double
futureValue 0 currentValue _ _ = currentValue
futureValue periods currentValue savingsPerPeriod ratePerPeriod =
  (futureValue (n-1) v s r)*(1+r) + s
  where n = periods
        v = currentValue
        s = savingsPerPeriod
        r = ratePerPeriod
