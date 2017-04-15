module Ha where

import Parse
import Prelude hiding (exp, pure)

data Prog = Prog [Eqn] 
    deriving Show
    
data Eqn = Eqn Name [Pat] Exp 
    deriving Show
    
data Pat = PNil | PVar Name | PCons Name Name 
    deriving Show
    
data Exp = Nil | Var Name | App Name [Exp] | Cons Exp Exp
    deriving Show
    
type Name = String

eqn :: Parser Eqn
eqn = Eqn .:. name .*. char ' ' *.. many (pat ..* spaces) .*. sym "=" *.. expr

pat :: Parser Pat
pat = PNil ... nil
	.|. PVar .:. name 
    .|. PCons .:. (sym "(" *.. name ..* sym ":") .*. (name ..* sym ")")

expr :: Parser Exp
expr = consOrApp .:. app .*. (Just .:. sym ":" *.. expr .|. pure Nothing)
    where
        consOrApp a (Just ex) = Cons a ex
        consOrApp a Nothing   = a

app :: Parser Exp
app = nameOrMany .:. name .*. spaces *.. many (arg ..* spaces)
    where
        nameOrMany n [] = Var n
        nameOrMany n args = App n args

arg :: Parser Exp
arg = Nil ... nil
    .|. Var .:. name
    .|. sym "(" *.. expr ..* sym ")"
     
nil :: Parser String
nil = sym "[]"

name :: Parser Name
name = many1 (lower .|. upper)

prog :: Parser Prog
prog = Prog .:. many1 eqn