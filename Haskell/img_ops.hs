import MatrixOps

--	image processing
data Matrix a = Shlogo [[a]]

instance (Show a) => Show (Matrix a) where
    show (Shlogo m)=
        let bind t = foldr (++) t -- we will end each join with the newline character
            formatLine = (bind "\n").(map ((++" ").show))
        in filter (/='\'') $ bind "" $ (map formatLine) m


l1="        ***** **            ***** **    "
l2="     ******  ****        ******  ****   "
l3="    **   *  *  ***      **   *  *  ***  "
l4="   *    *  *    ***    *    *  *    *** "
l5="       *  *      **        *  *      ** "
l6="      ** **      **       ** **      ** "
l7="      ** **      **       ** **      ** "
l8="    **** **      *      **** **      *  "
l9="   * *** **     *      * *** **     *   "
l10="      ** *******          ** *******    "
l11="      ** ******           ** ******     "
l12="      ** **               ** **         "
l13="      ** **               ** **         "
l14="      ** **               ** **         "
l15=" **   ** **          **   ** **         "
l16="***   *  *          ***   *  *          "
l17=" ***    *            ***    *           "
l18="  ******              ******            "
l19="    ***                 ***             "

logo = [l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19]

printImg img = putStrLn $ (show (Shlogo img))

--	operations
flipV = map reverse
flipH = reverse

rot90left = tr
rot90right = flipV.tr

ngt = map (map (\p -> if (p == '*') then ' ' else '*'))

addV = (++)
addH = zipWith (++)

cropH x y l = (reverse (drop y (reverse (drop x l))))
cropV x y l = map (cropH x y) l

overlap = zipWith (zipWith ov)
ov l1 l2 = if (l1 == ' ' && l2 == ' ') then ' ' else '*' 