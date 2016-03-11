module Main where
import RayMan



pairs (a:b:xs) = (a,b) : (pairs (b:xs))
pairs _ = []
					   
cube d trans material
    = let side (v1, v2, v3, v4)
	      = [Object (Triangle v1 v2 v3) material,
		 Object (Triangle v1 v3 v4) material]
	  v1 = trans $ Vector3 (-d) ( d) ( d)
	  v2 = trans $ Vector3 ( d) ( d) ( d)
	  v3 = trans $ Vector3 ( d) (-d) ( d)
	  v4 = trans $ Vector3 (-d) (-d) ( d)

	  v5 = trans $ Vector3 (-d) ( d) (-d)
	  v6 = trans $ Vector3 ( d) ( d) (-d)
	  v7 = trans $ Vector3 ( d) (-d) (-d)
	  v8 = trans $ Vector3 (-d) (-d) (-d)

	  in foldr (++) [] $ map side [(v4,v1,v2,v3),
				       (v3,v2,v6,v7),
				       (v7,v6,v5,v8),
				       (v8,v5,v1,v4),
			               (v1,v5,v6,v2),
				       (v8,v4,v3,v7)] 
	      

pyramid d trans material
    = let side ((x1,z1),(x2,z2)) = Object (Triangle (trans (Vector3 x1   0     (-z1)))
					            (trans (Vector3 0    (2*d) 0))
					            (trans (Vector3 x2   0     (-z2))))
				          material
	        in  map side $ pairs [((-d), (-d)),
				      (d,    (-d)),
				      (d,    d),
				      ((-d), d),
				      ((-d), (-d))]


pyramids = let onePyramid trans = pyramid 80.0 trans (OneColor (Color 1.0 0.0 0.0) 0.0)
	       translate a m = (#+ m) . (vector_rotateY a)
	       in foldr (++) [] $ map onePyramid [translate 0 (Vector3 (-300) 0 (-300)),
						  translate 0 (Vector3 300 0 (-300)),
						  translate 0 (Vector3 300 0 300),
						  translate 0 (Vector3 (-300) 0 300)]



view = View { view_center = Vector3 0 200 800,
	      view_direction = Vector3 0 (-1) (-6),
	      view_up = Vector3 0 1 0,
	      view_distance = 150,
	      view_width = 200,
	      view_height = 100,
	      view_transformation = vector_rotateY (pi/2) }


scene = Scene { scene_background =  Color 0.0 0.0 0.0,

		scene_objects =   pyramids
				  
                                  ++

                                  cube 100.0
				       ((#+ Vector3 0 100 0) . (vector_rotateY (pi/8)))
				       (OneColor (Color 0.0 0.0 0.7) 0.8)

				  ++

				  [Object (Plane (Vector3 0 1 0) 0)
					  (Marble (OneColor (Color 1.7 1.0 0.0) 0.5)
					          (OneColor (Color 0.0 0.0 0.0) 0.0)
					          200.0)],

		scene_lights = [ PointLight (Vector3 0 100 (-500))
			                    (200.0 `color_scalarMul` color_white)
			                    30.0,

                                 PointLight (Vector3 0 100 500)
				            (200.0 `color_scalarMul` color_white)
			                    30.0,

				 AmbientLight (Color 0.2 0.2 0.2)
			       ]
              }

columns = 400
rows = 200


main = do outputPpm columns rows (rayTraceScene scene view columns rows)