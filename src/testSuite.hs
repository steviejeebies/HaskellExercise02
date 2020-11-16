type Id = String

data Expr
  = Val Double
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Dvd Expr Expr
  | Var Id
  | Def Id Expr Expr
  deriving (Eq, Show)

type Dict k d  =  [(k,d)]

define :: Dict k d -> k -> d -> Dict k d
define d s v = (s,v):d

find :: Dict String d -> String -> Either String d
find []             name              =  Left ("undefined var "++name)
find ( (s,v) : ds ) name | name == s  =  Right v
                         | otherwise  =  find ds name

type EDict = Dict String Double

v42 = Val 42 ; j42 = Just v42

-- VARIABLES USED FOR TESTING -- 

x42 = ("x",42.0)                    -- dict entry, x = 42
y99 = ("y",99.0)                    -- dict entry, y = 99
emptyD   =  []         ::  EDict    -- empty dictionary
dx42     =  [x42]      ::  EDict    -- dict containing only x
dy99     =  [y99]      ::  EDict    -- dict containing only y
dx42y99  =  [y99,x42]  ::  EDict    -- dict containing x and y only
d0 = Val 0.0    -- raw values, decimal --
d1 = Val 1.0
d2 = Val 2.0
d42 = Val 42.0
d10 = Val 10.0
d99 = Val 99.0
d5 = Val 5.0
vx = Var "x"    -- variable, string name only --
vy = Var "y"
vz = Var "z"
dx42y7 = [("x",42.0),("y",7.0)] -- dict, x = 42, y = 7
n = "n"         -- just a string, should not be recognised -- 
va = Var "a"    -- variable names, strings only
vb = Var "b"
vc = Var "c"
vd = Var "d"
eBad = Dvd d1 d0    -- expression, divides by 0

-- DO NOT EDIT ABOVE THIS LINE -- 

eval :: EDict -> Expr -> Either String Double

eval _ (Val x) = Right x
eval d (Var i) = find d i     -- returns Left ("undefined") or Right v
eval d (Add x y) = evalOp d (+) x y
eval d (Mul x y) = evalOp d (*) x y
eval d (Sub x y) = evalOp d (-) x y
eval d (Dvd x (Val 0.0)) = Left "div by zero"
eval d (Dvd x y) = evalOp d (/) x y

evalOp d op x y = 
    let r = eval d x; s = eval d y
    in case (r,s) of
        (Right m, Right n)  -> Right (m `op` n)
        (Left m, _)         -> Left m
        (_, Left n)         -> Left n