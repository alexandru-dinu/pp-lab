-- courtesy of Matei Popovici

-- print matrix
-- map show on each element and then construct a string
getString l = ((foldr (\h t -> h ++ "\t" ++ t) "") . (map show)) l

-- constructs a string for each line and append a newline at the end
printMatrix m = putStr $ foldr (\line rest -> (getString line) ++ "\n" ++ rest) "" m


-- image operations
data Matrix a = Shlogo [[a]]

instance (Show a) => Show (Matrix a) where
    show (Shlogo m) =
        let
            bind t     = foldr (++) t
            formatLine = (bind "\n") . (map ((++" ").show))
        in filter (/='\'') $ bind "" $ map formatLine m


l1  = "        ***** **            ***** **    "
l2  = "     ******  ****        ******  ****   "
l3  = "    **   *  *  ***      **   *  *  ***  "
l4  = "   *    *  *    ***    *    *  *    *** "
l5  = "       *  *      **        *  *      ** "
l6  = "      ** **      **       ** **      ** "
l7  = "      ** **      **       ** **      ** "
l8  = "    **** **      *      **** **      *  "
l9  = "   * *** **     *      * *** **     *   "
l10 = "      ** *******          ** *******    "
l11 = "      ** ******           ** ******     "
l12 = "      ** **               ** **         "
l13 = "      ** **               ** **         "
l14 = "      ** **               ** **         "
l15 = " **   ** **          **   ** **         "
l16 = "***   *  *          ***   *  *          "
l17 = " ***    *            ***    *           "
l18 = "  ******              ******            "
l19 = "    ***                 ***             "

ppLogo = [l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16, l17, l18, l19]

printImg img = putStrLn $ show (Shlogo img)
