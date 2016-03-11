module Light where

import Vector
import Color
import Shape
import Ray

import Data.Maybe

data Lightning = Lightning { lightning_direction :: Maybe (Vector Float),
			     lightning_color :: Color }


data Light = PointLight { pointLight_position :: Vector Float,
		          pointLight_color :: Color,
			  pointLight_radius :: Float }

             | AmbientLight { ambientLight_color :: Color }

light_giveLight :: Light -> Vector Float -> [Shape] -> Lightning
light_giveLight light@(PointLight _ _ _) target shapes
	   = let ray = Ray (pointLight_position light) (normalize $ target #- pointLight_position light)
		 distance = Vector.length (target #- pointLight_position light)
		 maybeClosestIntersection = shape_closestIntersection shapes ray
		 colorAtDistance = (1 / (1 + (distance/pointLight_radius light)^2)) `color_scalarMul` pointLight_color light
		 color = case maybeClosestIntersection of Nothing -> colorAtDistance
						          Just (_,closestIntersection) -> if intersection_t closestIntersection < distance - 0.01
											     then color_black
										             else colorAtDistance


		 in Lightning (Just $ ray_direction ray) color

light_giveLight light@(AmbientLight _) target objects
    = Lightning Nothing (ambientLight_color light)
