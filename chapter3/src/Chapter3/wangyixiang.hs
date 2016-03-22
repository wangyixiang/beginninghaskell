doublev1 list = map (\x-> x*2) list
doublev2      = \list -> map (\x->x*2) list

-- Partial Application of a Function
doublev3      = map (\x-> x*2)

doublev4      = map (*2)

duplicateOddsv1 list = map (*2) $ filter odd list

-- point-free style
duplicateOddsv2      = map (*2) . filter odd
