module Beta where


redex1 = (\ x -> x) "Hello"

redex2 = (\ x f -> f x x) (True && False)
