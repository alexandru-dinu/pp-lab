-- courtesy of Matei Popovici

-- a String denotes a name (variable)

data Lambda = Var String | Lambda String Lambda | App Lambda Lambda

instance Show Lambda where
	show (Var x) = show x
	show (Lambda x l) = "λ" ++ show x ++ "." ++ show l
	show (App e1 e2) = "(" ++ show e1 ++ ") " ++ "(" ++ show e2 ++ ")"


l1 = App (Lambda "x" (Var "x")) (Var "y")
l2 = (App (Lambda "x" (Var "z")) (App (Lambda "x" (Var "x")) (App (Var "y") (Var "y"))))

-- e[e'/x]
-- expression e where all occurences of x are replaced with e'
-- substitute x with e' in e
-- substitute _ with _ in _
subst :: String -> Lambda -> Lambda -> Lambda
subst x e (Var v) = if x == v then e else (Var v)
subst x e (App e1 e2) = App (subst x e e1) (subst x e e2)
subst x e (Lambda y e') = if x == y then (Lambda y e') 
                                    else if y `elem` (fv e) then subst x e (Lambda "M" (subst y (Var "M") e'))
                                                            else Lambda y (subst x e e')


-- free variables
fv :: Lambda -> [String]
fv (Var v) = [v]
fv (Lambda x e) = filter (/= x) (fv e)
fv (App e e') = (fv e) ++ (fv e')

-- bounded variables
bv :: Lambda -> [String]
bv (Var v) = []
bv (Lambda x e) = x:(bv e)
bv (App e e') = [v | v <- (bv e), not (v `elem` (fv e'))] ++ [v | v <- (bv e'), not (v `elem` (fv e))]

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


