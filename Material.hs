module Material where
import Color
import Vector
import Ray
import Light

data Material = OneColor { oneColor_color :: Color,
			   oneColor_reflectivity :: Float } | 

		Marble { marble_material1 :: Material,
			 marble_material2 :: Material,
			 marble_size :: Float }



material_emitColor (OneColor color _) intersection lightning =
    directionMultiplier intersection lightning `color_scalarMul` color %* lightning_color lightning

material_emitColor marble intersection lightning
    | marbleFunction marble intersection
	= material_emitColor (marble_material1 marble) intersection lightning
    | otherwise
	= material_emitColor (marble_material2 marble) intersection lightning




material_reflect (OneColor _ reflectivity) intersection lightning =
    (directionMultiplier intersection lightning * reflectivity) `color_scalarMul` lightning_color lightning

material_reflect marble@(Marble _ _ _ ) intersection lightning
    | marbleFunction marble intersection
	= material_reflect (marble_material1 marble) intersection lightning
    | otherwise
	= material_reflect (marble_material2 marble) intersection lightning



-- UTILS

directionMultiplier intersection lightning =
    case lightning_direction lightning of Nothing -> 1.0
				          Just direction -> max 0.0 (normalize (intersection_normal intersection)
								     #. ((-1) #* normalize direction))

marbleFunction marble intersection = 
    (even (truncate (sin(vector_x (intersection_point intersection) *pi/marble_size marble)
		     + vector_z (intersection_point intersection) /marble_size marble)))
