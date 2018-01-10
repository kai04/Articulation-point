ak_mul x y | x==1 = x*y
            | odd x = (ak_mul (quot x 2) y*2)+y
            | otherwise = ak_mul (quot x 2) y*2			