module RayMan (Scene(..),
	       View(..),
	       module Vector,
	       module Color,
	       module Ray,
	       module Shape,
	       module Material,
	       module Object,
	       module Light,
	       rayTraceScene,
	       outputPpm
	      ) where

import Vector
import Color
import Ray
import Light
import Shape
import Material
import Object

import Maybe



data Scene = Scene { scene_background :: Color,
		     scene_objects :: [Object],
		     scene_lights :: [Light] }

data View = View { view_center :: Vector Float,
		   view_direction :: Vector Float,
		   view_up :: Vector Float,
		   view_distance :: Float,
		   view_width :: Float,
		   view_height :: Float,
		   view_transformation :: Vector Float -> Vector Float}


viewRays :: View -> Float -> Float -> [Ray]
viewRays view columns rows =
    let center = (view_transformation view) (view_center view)
	up = (view_transformation view) (view_up view)
	direction = (view_transformation view) (view_direction view)
        screenDepth = setLength (view_distance view) direction
        rayBase = center #- screenDepth
	screenRight = normalize (direction `cross` up)
	screenUp = normalize (screenRight `cross` direction)

	in [ Ray rayBase ( screenDepth #+ (((x - columns / 2) * view_width view / columns) #* screenRight)
	                     #- (((y - rows / 2) * view_height view / rows) #* screenUp) )
	     | y <- [0 .. rows - 1], x <- [0 .. columns -1] ]


rayTrace depth scene ray
	 | depth < 1 = color_black
	 | otherwise = 
    let	sumDiffuse (object,intersection)
	    = color_sum $ map (\light -> material_emitColor (object_material object)
			                                    intersection
			                                    (light_giveLight light
				                                             (intersection_point intersection)
				                                             (map object_shape (scene_objects scene))
							    )
			      )
			      (scene_lights scene)

        reflectedDirection intersection =
	    let iNormal = normalize (intersection_normal intersection)
		dirInReverse = (-1) #* ray_direction ray
		in 2 #* (((iNormal #. dirInReverse) #* iNormal) #+ ray_direction ray )
		       #+ dirInReverse

        reflection intersection =
	    rayTrace (depth - 1) scene (Ray (intersection_point intersection) (reflectedDirection intersection))

	reflectedColor (object,intersection) = 
	    material_reflect (object_material object)
			     intersection
			     (Lightning { lightning_direction = Just $ (-1) #* reflectedDirection intersection,
					  lightning_color = reflection intersection })

        choseColor mi = case mi of Nothing -> scene_background scene
				   Just i -> sumDiffuse i %+ reflectedColor i

        in choseColor $ object_closestIntersection (scene_objects scene) ray

rayTraceScene scene view columns rows =
        map (rayTrace 2 scene)
	    (viewRays view (fromInteger columns) (fromInteger rows))



outputPpm columns rows colors = putStr $ "P3\n"
 				++ show columns ++ " " ++ show rows
				++ "\n255\n"
				++ foldr (++) "" (map (\color -> show color ++ " ") colors)

