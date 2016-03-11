module Shape where

import Vector
import Ray
import FIO

import Maybe(fromJust,catMaybes)
import List(sortBy,minimumBy)

data Shape = Plane { plane_normal :: Vector Float,
		     plane_distance :: Float } |

	     Triangle { triangle_v1 :: Vector Float,
		        triangle_v2 :: Vector Float,
		        triangle_v3 :: Vector Float }


shape_closestIntersection shapes ray = fIOMapMaybeMinimum (flip shape_intersect ray) shapes


-- PLANE

shape_intersect plane@(Plane _ _) ray
    | vd < 0 = let t = - ( unit_plane_normal #. ray_origin ray + plane_distance plane ) / vd
		   in if t < 0 then Nothing
		               else Just Intersection { intersection_point = t #* unit_ray_direction #+ ray_origin ray,
							intersection_normal = unit_plane_normal,
							intersection_t = t }

    | otherwise = Nothing
    where vd = plane_normal plane #. unit_ray_direction
	  unit_ray_direction = normalize (ray_direction ray)
	  unit_plane_normal = normalize (plane_normal plane)



-- TRIANGLE

shape_intersect triangle@(Triangle _ _ _) ray
                       = let planeNormal = normalize $ (triangle_v3 triangle #- triangle_v1 triangle) `cross` 
                        		                   (triangle_v2 triangle #- triangle_v3 triangle)
			     planeD = - (triangle_v1 triangle #. planeNormal)
			     intersection = shape_intersect (Plane planeNormal planeD) ray

                             [axis1,axis2] = tail $ reverse $ sortBy (\axis1 axis2 -> abs(axis1 planeNormal) `compare` abs(axis2 planeNormal))
					                             [vector_x, vector_y, vector_z]
		             project vector = Vector2 { vector_x = axis1 vector,
							vector_y = axis2 vector }
			     triangleVectorPairs = [ (triangle_v1 triangle, triangle_v2 triangle),
						     (triangle_v2 triangle, triangle_v3 triangle),
						     (triangle_v3 triangle, triangle_v1 triangle) ]
			     projectedTriangleVectorPairs = map (\(v1,v2) -> (project v1, project v2)) triangleVectorPairs
			     projectedIntersection = project $ intersection_point (fromJust intersection)

			     sideIntersects v (lv1, lv2) = let j = (vector_x lv2 - vector_x lv1)
							       k = (vector_y lv2 - vector_y lv1) / j
							       d = vector_y lv1 - k * vector_x lv1
							       in if (vector_y v > min (vector_y lv1) (vector_y lv2)
									    && vector_y v < max (vector_y lv1) (vector_y lv2))
			                                             then if j == 0 then vector_x lv1 > vector_x v
 							                            else (vector_y v - d) / k > vector_x v
								     else False
							         
			     in case intersection of Nothing -> Nothing
					             _ -> if Prelude.length (filter (==True) (map (sideIntersects projectedIntersection)
											      projectedTriangleVectorPairs)) == 1
							     then intersection
							     else Nothing
