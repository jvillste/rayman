module Vector where
import Prelude hiding (length)

data Vector a =  Vector3 {vector_x, vector_y, vector_z :: a} | Vector2 {vector_x, vector_y :: a}
		 deriving (Show, Eq)

(Vector3 a b c) #. (Vector3 x y z) = a*x + b*y + c*z
(Vector2 a b) #. (Vector2 x y) = a*x + b*y

(Vector3 a b c) #+ (Vector3 x y z) = Vector3 (a+x) (b+y) (c+z)
(Vector2 a b) #+ (Vector2 x y) = Vector2 (a+x) (b+y)

(Vector3 a b c) #- (Vector3 x y z) = Vector3 (a-x) (b-y) (c-z)
(Vector2 a b) #- (Vector2 x y) = Vector2 (a-x) (b-y)

a #* (Vector3 x y z) = Vector3 (a*x) (a*y) (a*z)
a #* (Vector2 x y) = Vector2 (a*x) (a*y)

(Vector3 x y z) `cross` (Vector3 a b c) = Vector3 (y*c - z*b) (z*a - x*c) (x*b - y*a)

length (Vector3 x y z) = sqrt(x*x + y*y + z*z)
length (Vector2 x y) = sqrt(x*x + y*y)

normalize v@(Vector3 x y z) = Vector3 (x / length v) (y / length v) (z / length v)
normalize v@(Vector2 x y) = Vector2 (x / length v) (y / length v)

setLength l v = l #* normalize v


vector_rotateZ q (Vector3 x y z) = Vector3 (x*cos q - y*sin q)
					   (x*sin q + y*cos q)
					   (z)

vector_rotateY q (Vector3 x y z) = Vector3 (z*sin q + x*cos q)
				           (y)
					   (z*cos q - x*sin q)
					   