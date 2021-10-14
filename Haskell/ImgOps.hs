import MatrixOps (transpose)

--	image processing
data Matrix a =
  Shlogo [[a]]

instance (Show a) => Show (Matrix a) where
  show (Shlogo m) =
    let bind t = foldr (++) t -- we will end each join with the newline character
        formatLine = (bind "\n") . (map ((++ " ") . show))
     in filter (/= '\'') $ bind "" $ (map formatLine) m

logo =
  [ "        ***** **            ***** **    "
  , "     ******  ****        ******  ****   "
  , "    **   *  *  ***      **   *  *  ***  "
  , "   *    *  *    ***    *    *  *    *** "
  , "       *  *      **        *  *      ** "
  , "      ** **      **       ** **      ** "
  , "      ** **      **       ** **      ** "
  , "    **** **      *      **** **      *  "
  , "   * *** **     *      * *** **     *   "
  , "      ** *******          ** *******    "
  , "      ** ******           ** ******     "
  , "      ** **               ** **         "
  , "      ** **               ** **         "
  , "      ** **               ** **         "
  , " **   ** **          **   ** **         "
  , "***   *  *          ***   *  *          "
  , " ***    *            ***    *           "
  , "  ******              ******            "
  , "    ***                 ***             "
  ]

printImg img = putStrLn $ (show (Shlogo img))

--	operations
flipV = map reverse

flipH = reverse

rot90left = transpose

rot90right = flipV . transpose

ngt =
  map
    (map
       (\p ->
          if (p == '*')
            then ' '
            else '*'))

addV = (++)

addH = zipWith (++)

cropH x y l = (reverse (drop y (reverse (drop x l))))

cropV x y l = map (cropH x y) l

overlap = zipWith (zipWith ov)
  where
    ov l1 l2 =
      if (l1 == ' ' && l2 == ' ')
        then ' '
        else '*'
