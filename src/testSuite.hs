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

-- raw values, decimal --
d0 = Val 0.0
d1 = Val 1.0
d2 = Val 2.0
d42 = Val 42.0
d10 = Val 10.0
d99 = Val 99.0
d5 = Val 5.0

-- variable, string name only --
vx = Var "x"
vy = Var "y"
vz = Var "z"

-- dict, x = 42, y = 7
dx42y7 = [("x",42.0),("y",7.0)]

-- just a string, should not be recognised -- 
n = "n"

-- variable names, strings only
va = Var "a"
vb = Var "b"
vc = Var "c"
vd = Var "d"

-- DO NOT EDIT ABOVE THIS LINE -- 

eval :: EDict -> Expr -> Either String Double

eval _ (Val x) = Right x

eval d e = error "test failed"
eval d (Var i) = find d i     -- returns Left ("undefined") or Right v