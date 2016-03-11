module Ray where
import Vector

data Ray = Ray { ray_origin :: Vector Float,
		 ray_direction :: Vector Float }


data Intersection = Intersection { intersection_point :: Vector Float,
				   intersection_normal :: Vector Float,
				   intersection_t :: Float }

instance Eq Intersection where
    i1 == i2 = intersection_t i1 == intersection_t i2

instance Ord Intersection where
    i1 <= i2 = intersection_t i1 <= intersection_t i2
