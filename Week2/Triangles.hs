module Triangles where


data Shape = NoTriangle | Equilateral 
            | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z 
	| x >= (y+z) 
	  || y >= (x+z) 
	  || z >= (x+y) 		= NoTriangle 
	| x == y && y == z 		= Equilateral
	| x^2 + y^2 == z^2 
	  || y^2 + z^2 == x^2 
	  || z^2 + x^2 == y^2 		= Rectangular
	| x == y || y == z || z == x 	= Isosceles
	| otherwise			= Other
