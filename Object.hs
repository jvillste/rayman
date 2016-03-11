module Object where

import Shape
import Material
import Ray
import FIO

import Data.List
import Data.Maybe

data Object = Object { object_shape :: Shape,
		       object_material :: Material }

data ObjectIntersection = ObjectIntersection { objectIntersection_object :: Object,
					       objectIntersection_intersection :: Intersection }


object_intersect object ray = shape_intersect (object_shape object) ray

object_closestIntersection objects ray = fIOMapMaybeMinimum (flip object_intersect ray) objects
