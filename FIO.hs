module FIO where

import Data.Maybe(catMaybes)
import Data.List(minimumBy)

liftFIO f = \(_, o) -> f o
liftFIO2 f = \(_, o1) (_, o2) -> f o1 o2
fIOMap f is = zip is (map f is)
raiseFIOMaybe = \(i,mo) -> case mo of Nothing -> Nothing
				      Just o -> Just (i, o)
fIOO (i,o) = o
fIOI (i,o) = i

listFToMaybe f = \i -> case i of [] -> Nothing
				 _ -> Just (f i)

fIOMapMaybeMinimum f is = 
    let fIOs = catMaybes $ map raiseFIOMaybe $ fIOMap f is

        in (listFToMaybe $ minimumBy $ liftFIO2 (compare)) fIOs
