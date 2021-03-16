h :: (Integer, Integer, Integer) -> Integer
h (x, y, z) = x + y + z

h' :: Integer -> (Integer -> (Integer -> Integer))
h' x y z = x + y + z