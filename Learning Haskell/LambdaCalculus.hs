--To run:
-- ghci
-- :l LambdaCalculus.hs
-- try something, like freeVars (App (Var "x") (Var "x"))

module Interpreter where
import Control.Exception
import Debug.Trace
import Data.List
import Data.Set
import Data.Maybe

---- Data types ----

type Name = String

data Expr = 
  Var Name
  | Lambda Name Expr
  | App Expr Expr
  deriving 
    (Eq,Show)

---- Pseudocode ----

{-
interpret(x) = Nothing
interpret(\x.E1) = if (E1 is reducible) then \x.interpret(E1) else Nothing
interpret(E1 E2) = let f = interpret(E1)
                       a = interpret(E2)
                   in case f of
                   	   Just x -> (App x a)
                       Nothing -> in case a of
                       	   Just y -> (App f y)
                       	   Nothing-> in case E1 of
                       	   	   \b.E3 -> interpret(E3[a/b])
                       	   	   otherwise -> Nothing
-}

---- Functions ----

--Get list of free variables in some expression
--For example, freeVars (App (Var "x") (Var "x")) yields ["x"]
freeVars::Expr -> [Name]
freeVars (Var x) = [x]                                  --A lone variable is free
freeVars (Lambda x e1) = (freeVars e1) Data.List.\\ [x] --All the free variables in e1 are free in e, except the one which the lambda binds
freevars (App e1 e2) = Data.Set.toList (Data.Set.union (Data.Set.fromList $ freeVars e1) (Data.Set.fromList $ freeVars e2)) --The union of free vars in e1 and e2 are free in e



--Get a list of unused variables which can be used in substitution
--For example, freshVars [Lambda "1_" (App (Var "x") (App (Var "1_") (Var "2_")))] yields the infinite list [1_,3_,4_,5_,..]
freshVars :: [Expr] -> [Name]
freshVars expr_li = 
    (Data.List.map (++ "_") (Data.List.map show [1..]))                     --Generates an infinite list: ["1_", "2_", "3_", ...
    Data.List.\\ (Data.List.foldl (++) [] (Data.List.map freeVars expr_li)) --Remove from it all of the used free variables in the provided expressions



--Get a function which substitutes some variable for an expression, inside of some other expression
subst::(Name,Expr) -> Expr -> Expr
subst (x,m) ee =
    case ee of
        Var y -> if (y==x) then m else Var y                         --If the espression we're replacing in is a variable, simply compare and replace if necessary
        App e1 e2 -> App (subst (x,m) e1) (subst (x,m) e2)           --If it's an application, just recursively substitute on the two parts
        Lambda y e1 ->                                               --in a lambda expression, it's more complex since we must avoid collisions with bound variables
            if (y==x)
                then (Lambda y e1)                                   --If the variable bound by the lambda is the one being replaced, we can safely ignore it
            else
                (let z = (Data.List.head (freshVars [e1,(Var x),m])) --Otherwise, we choose an unused variable to substitute,
                in (Lambda z (subst (x,m) (subst (y,Var z) e1))))    --and we use it to replace the bound variable.



--Reduce an expression once, according to applicative order
appNF_OneStep::Expr -> Maybe Expr
appNF_OneStep (Var e) = Nothing                      --Lone variables never have redexes
appNF_OneStep (Lambda a b) =                         --In a lambda expression,
    (case (appNF_OneStep b) of
        Nothing -> Nothing                         --The only way for there to be a redex,
        Just x -> Just (Lambda a x))               --is if the part "after the period" contains some nested redex.
appNF_OneStep (App a b) =                            --Applications are the most obvious example of redexes.
    case (appNF_OneStep a) of
        Just x -> Just (App x b)                   --If the left side has a redex inside, apply it.
        Nothing -> case (appNF_OneStep b) of
            Just y -> Just (App a y)               --If the left didn't and the right does, then apply that one.
            Nothing -> case a of
                Var u -> Nothing
                Lambda u v -> Just (subst (u,b) v) --If this application itself is a valid regex, then beta-reduce.
                App u v -> Nothing



--Reduce an expression n times over, and return the result.
appNF_n::Int -> Expr -> Expr
appNF_n n i =
    (let ret = (last $ Data.List.filter (/= Nothing)            {-Set ret to the last Expr,
    	-} $ Data.List.take (n+1) $ iterate stepMaybe (Just i)) --in a list resembling [Just i, iReduced1x, iReduced2x, Nothing]
    in (case ret of
        Just x -> x                                             --Peel off the Just wrapper in the returned result
        Nothing -> i))



--Helper function: runs one step of reduction on an expression nested in a Maybe
stepMaybe::Maybe Expr -> Maybe Expr
stepMaybe m = case m of
    Just x -> appNF_OneStep x --Apply reduction on whatever is inside
    Nothing -> Nothing        --return Nothing if the input is Nothing
