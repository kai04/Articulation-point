coeffs a 0 = (1, 0)
coeffs a b = let (q, r) = a `quotRem` b
                 (x, y) = coeffs b r
             in  (y, x - q * y)