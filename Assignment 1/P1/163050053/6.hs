ak_mult x y | x==1 = x*y
            | odd x = (ak_mult (quot x 2) y*2)+y
            | otherwise = ak_mult (quot x 2) y*2			