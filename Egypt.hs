egyptianFactions :: Integer -> Integer -> [(Integer,Integer)]
egyptianFactions m n = foldl gleedFunction [] [(m,n)]
	where gleedFunction acc (x,y) = let (jo,amari) = ((y `div` x)  ,(y `rem` x))
	                                in if amari == 0 then
	                                      (1,y):acc
	                                   else
	                                      let (x',y') = (x * (jo + 1) - y , y * (jo + 1))
	                                      in  gleedFunction ((1,jo + 1):acc) (x',y')
