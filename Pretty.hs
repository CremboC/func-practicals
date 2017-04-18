-- Philip Wadler's 2003 "Prettier Printer", with only minor modifications.

module Pretty (DOC, nil, text, line, nest, (<>), group, pretty,
              (<+>), (</>), folddoc, spread, stack, bracket,
              (<+/>), fillwords, fill) where

data Doc  =  Nil | String `Text` Doc | Int `Line` Doc

layout :: Doc -> String
layout Nil           =  ""
layout (s `Text` x)  =  s ++ layout x
layout (i `Line` x)  =  '\n' : replicate i ' ' ++ layout x

data DOC  =  NIL | TEXT String | LINE | DOC :<> DOC | NEST Int DOC 
          |  DOC :<|> DOC

nil :: DOC
nil  =  NIL

text :: String -> DOC
text s  =  TEXT s

line :: DOC
line  =  LINE

(<>) :: DOC -> DOC -> DOC
x <> y  =  x :<> y

nest :: Int -> DOC -> DOC
nest i x  =  NEST i x

group :: DOC -> DOC
group x  =  flatten x :<|> x

flatten :: DOC -> DOC
flatten NIL         =  NIL
flatten (x :<> y)   =  flatten x :<> flatten y
flatten (NEST i x)  =  flatten x
flatten (TEXT s)    =  TEXT s
flatten LINE        =  TEXT " "
flatten (x :<|> y)  =  flatten x

pretty :: Int -> DOC -> String
pretty w x  =  layout (best w 0 x)

best :: Int -> Int -> DOC -> Doc
best w k x  =  be w k [(0,x)]

be :: Int -> Int -> [(Int, DOC)] -> Doc
be w k []                =  Nil
be w k ((i,NIL):z)       =  be w k z
be w k ((i,x :<> y):z)   =  be w k ((i,x):(i,y):z)
be w k ((i,NEST j x):z)  =  be w k ((i+j,x):z)
be w k ((i,TEXT s):z)    =  s `Text` be w (k+length s) z
be w k ((i,LINE):z)      =  i `Line` be w i z
be w k ((i,x :<|> y):z)  =  better w k (be w k ((i,x):z)) (be w k ((i,y):z))

better :: Int -> Int -> Doc -> Doc -> Doc
better w k x y  =  if fits (w-k) x then x else y

fits :: Int -> Doc -> Bool
fits w x | w < 0     =  False
fits w Nil           =  True
fits w (s `Text` x)  =  fits (w - length s) x
fits w (i `Line` x)  =  True

-- Extensions beyond the core:

(<+>) :: DOC -> DOC -> DOC
x <+> y  =  x <> text " " <> y

(</>) :: DOC -> DOC -> DOC
x </> y  =  x <> line <> y

folddoc :: (DOC -> DOC -> DOC) -> [DOC] -> DOC
folddoc f []      =  nil
folddoc f [x]     =  x
folddoc f (x:xs)  =  f x (folddoc f xs)

spread :: [DOC] -> DOC
spread  =  folddoc (<+>)

stack :: [DOC] -> DOC
stack   =  folddoc (</>)

bracket :: String -> DOC -> String -> DOC
bracket l x r  =  group lxr
  where
  lxr  =  text l <> nest 2 (line <> x) <> line <> text r

(<+/>) :: DOC -> DOC -> DOC
x <+/> y  =  x <> group line <> y

fillwords :: String -> DOC
fillwords  =  folddoc (<+/>) . map text . words

fill :: [DOC] -> DOC
fill []        =  nil
fill [x]       =  x
fill (x:y:zs)  =  (flatten x <+> fill (flatten y : zs))
                  :<|>
                  (x </> fill (y : zs))

