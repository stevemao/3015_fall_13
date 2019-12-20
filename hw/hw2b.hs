

-- You need to write the functions curry3 and uncurry3 and add them to
-- this file.

curry f x y = f (x,y)
uncurry f (x,y) = f x y

plus1 x y  = x + y 
plus2 (x, y)  = x + y 

-- See http://en.wikipedia.org/wiki/Discriminant for a discussion of
-- the discriminant of a quadratic polynomial (a polynomial of the
-- form ax^2 + bx + c = 0.)  The delta function below computes the
-- discriminant of a quadratic polynomial.

delta (a,b,c) = sqrt (b ^ 2 - 4 * a * c)


