module HaParser where

import Prelude hiding (exp,pure)
import Parse
import Pretty
import Data.Maybe (isJust, isNothing, catMaybes)
import Data.Foldable (forM_)

import Test.LeanCheck

data Prog = Prog [Eqn]
    deriving Show
data Eqn = Eqn Name [Pat] Exp 
    deriving Show
data Pat = PNil | PVar Name | PCons Name Name
    deriving Show
data Exp = Nil | Var Name | App Name [Exp] | Cons Exp Exp
    deriving Show
type Name = String


instance Listable Exp where
    tiers = cons0 Nil \/ cons1 Var \/ cons2 App \/ cons2 Cons
    
instance Listable Pat where
    tiers = cons0 PNil \/ cons1 PVar \/ cons2 PCons
    
instance Listable Eqn where
    tiers = cons3 Eqn
    
instance Listable Prog where
    tiers = cons1 Prog

name :: Parser Name
name = many1 (lower .|. upper) ..* spaces

-- prog :: Parser Prog
-- prog = Prog .:. many1 (eqn ..* line)

eqn :: Parser Eqn
eqn = Eqn .:. name ..* spaces .*. many pat .*. (sym "=" *.. exp)

pat :: Parser Pat
pat = PNil ... sym "[]"
        .|. PVar .:. name
        .|. PCons .:. sym "(" *.. name .*. sym ":" *.. name ..* sym ")"
        
exp :: Parser Exp
exp = consOrApp .:. app .*. ((Just .:. sym ":" *.. exp) .|. pure Nothing)
    where 
        consOrApp a (Just e) = Cons a e
        consOrApp a Nothing  = a

app :: Parser Exp
app = varOrApp .:. name .*. many arg
    where
        varOrApp n [] = Var n
        varOrApp n args = App n args
        
arg :: Parser Exp
arg = Nil ... string "[]"
        .|. Var .:. name
        .|. sym "(" *.. exp ..* sym ")"
  
eqnDoc :: Eqn -> DOC
eqnDoc (Eqn n [] exp)   = group (nameDoc n </> text "=" <> line) <> nest 2 (expDoc exp)
eqnDoc (Eqn n pats exp) = group (nameDoc n </> eqnPatsDoc </> text "=" <> line) <> nest 2 (expDoc exp)
    where eqnPatsDoc = fill (map patDoc pats)
        
patDoc :: Pat -> DOC
patDoc PNil           = text "[]"
patDoc (PVar name)    = nameDoc name
patDoc (PCons n1 n2)  = group (text "(" <> nameDoc n1 <> text ":" <> nameDoc n2 <> text ")")
        
expDoc :: Exp -> DOC
expDoc Nil             = text "[]"
expDoc (Var name)      = nameDoc name
expDoc (App name exps) = nest 4 $ group (text "(" <> nameDoc name </> expExpsDoc <> text ")")
    where expExpsDoc = fill (map expDoc exps)
expDoc (Cons e1 e2)    = group (text "(" <> expDoc e1 <> text ":" <> expDoc e2 <> text ")")

nameDoc :: Name -> DOC
nameDoc n = text n
        
prettyProg :: Int -> Prog -> String
prettyProg n p = ""

prettyEqn :: Int -> Eqn -> String
prettyEqn n = pretty n . eqnDoc
        
main = do
    let parser eq = parseWith eqn eq
    results <- catMaybes . map parser . lines <$> readFile "prog.ha"
    forM_ (map (prettyEqn 25) results) (\e -> putStrLn (e ++ "\n"))
    -- print $ results