module Color where

data Color = Color { color_r :: Float,
		     color_g :: Float,
		     color_b :: Float }

color_cut color = Color { color_r = min 1.0 (color_r color),
			  color_g = min 1.0 (color_g color),
			  color_b = min 1.0 (color_b color) }

color_sum = foldr (%+) color_black

c1 %+ c2 = Color { color_r = color_r c1 + color_r c2,
		   color_g = color_g c1 + color_g c2,
		   color_b = color_b c1 + color_b c2 }

c1 %* c2 = Color { color_r = color_r c1 * color_r c2,
		   color_g = color_g c1 * color_g c2,
		   color_b = color_b c1 * color_b c2 }

l `color_scalarMul` c =  Color { color_r = color_r c * l,
				 color_g = color_g c * l,
				 color_b = color_b c * l }

instance Show Color where
    show color = let c = color_cut color
		     in show (truncate ((color_r c) * 255))
			++ " " ++ show (truncate ((color_g c) * 255))
		        ++ " " ++ show (truncate ((color_b c) * 255))


color_black = Color 0.0 0.0 0.0
color_white = Color 1.0 1.0 1.0