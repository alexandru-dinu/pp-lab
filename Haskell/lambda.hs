-- courtesy of Matei Popovici

-- a String denotes a name (variable)

data Lambda = Var String | Lambda String Lambda | App Lambda Lambda

instance Show Lambda where
	show (Var x) = show x
	show (Lambda x l) = "λ" ++ show x ++ "." ++ show l
	show (App e1 e2) = "(" ++ show e1 ++ ") " ++ "(" ++ show e2 ++ ")"


l1 = App (Lambda "x" (Var "x")) (Var "y")
l2 = (App (Lambda "x" (Var "z")) (App (Lambda "x" (Var "x")) (App (Var "y") (Var "y"))))

-- useful for testing normal / applicative termination
e1 = Lambda "x" (Var "z")
e2 = App (Lambda "x" (App (Var "x") (Var "x"))) (Lambda "y" (App (Var "y") (Var "y")))
l3 = App e1 e2
-- normal l3 will stop
-- applicative l3 will not stop



-- e[e'/x]
-- expression e where all occurences of x are replaced with e'
-- substitute x with e' in e
-- substitute _ with _ in _
subst :: String -> Lambda -> Lambda -> Lambda
-- x[e/x] is e
-- y[e/x] is y
subst x e (Var y) = if x == y then e else (Var y)

-- (e1 e2)[e/x] is (e1[e/x] e2[e/x])
subst x e (App e1 e2) = App (subst x e e1) (subst x e e2)

-- {\x.e'}[e/x] is \x.e'
-- {\y.e'}[e/x] is \y.e'[e/x] if y is not free in e
subst x e (Lambda y e') = if x == y then (Lambda y e') 
                                    else if y `elem` (fv e) then subst x e (Lambda "M" (subst y (Var "M") e'))
                                                            else Lambda y (subst x e e')


-- free variables
fv :: Lambda -> [String]
fv (Var v) = [v]
fv (Lambda x e) = filter (/= x) (fv e)
fv (App e e') = (fv e) ++ (fv e')

-- bound variables
bv :: Lambda -> [String]
bv (Var v) = []
bv (Lambda x e) = x:(bv e)
bv (App e e') = [v | v <- (bv e), not (v `elem` (fv e'))] ++ 
				[v | v <- (bv e'), not (v `elem` (fv e))]
 

-- normal evaluation
normal :: Lambda -> Lambda
-- substitute x with e' in e
normal (App (Lambda x e) e') = normal (subst x e' e)
normal (App e e') = case normal e of
                      (Lambda x e'') -> normal (App (Lambda x e'') e')
                      _ -> (App e e')
normal e = e

-- applicative evaluation
applicative :: Lambda -> Lambda
-- TFUN and TAPP2
applicative (App (Lambda x e) e') = case e' of
                                      (App (Lambda _ _) _) -> applicative (App (Lambda x e) (applicative e'))
                                      _ -> applicative (subst x e' e)
-- TAPP1                                      
applicative (App e e') = case applicative e of
                      (Lambda x e'') -> applicative (App (Lambda x e'') e')
                      _ -> (App e e')
applicative e = e


