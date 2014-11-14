{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintCmm where

-- pretty-printer generated by the BNF converter

import AbsCmm
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: [a] -> Doc
  prtList = concatD . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))
  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])



instance Print Program where
  prt i e = case e of
   Prog functions -> prPrec i 0 (concatD [prt 0 functions])


instance Print Function where
  prt i e = case e of
   Fun type' id decls stms -> prPrec i 0 (concatD [prt 0 type' , prt 0 id , doc (showString "(") , prt 0 decls , doc (showString ")") , doc (showString "{") , prt 0 stms , doc (showString "}")])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Decl where
  prt i e = case e of
   Dec type' ids -> prPrec i 0 (concatD [prt 0 type' , prt 0 ids])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Stm where
  prt i e = case e of
   SDecl decl -> prPrec i 0 (concatD [prt 0 decl , doc (showString ";")])
   SExp exp -> prPrec i 0 (concatD [prt 0 exp , doc (showString ";")])
   SBlock stms -> prPrec i 0 (concatD [doc (showString "{") , prt 0 stms , doc (showString "}")])
   SWhile exp stm -> prPrec i 0 (concatD [doc (showString "while") , doc (showString "(") , prt 0 exp , doc (showString ")") , prt 0 stm])
   SReturn exp -> prPrec i 0 (concatD [doc (showString "return") , prt 0 exp , doc (showString ";")])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Exp where
  prt i e = case e of
   EAss id exp -> prPrec i 0 (concatD [prt 0 id , doc (showString "=") , prt 0 exp])
   ELt exp0 exp -> prPrec i 1 (concatD [prt 2 exp0 , doc (showString "<") , prt 2 exp])
   EAdd exp0 exp -> prPrec i 2 (concatD [prt 2 exp0 , doc (showString "+") , prt 3 exp])
   ESub exp0 exp -> prPrec i 2 (concatD [prt 2 exp0 , doc (showString "-") , prt 3 exp])
   EMul exp0 exp -> prPrec i 3 (concatD [prt 3 exp0 , doc (showString "*") , prt 4 exp])
   Call id exps -> prPrec i 4 (concatD [prt 0 id , doc (showString "(") , prt 0 exps , doc (showString ")")])
   EVar id -> prPrec i 4 (concatD [prt 0 id])
   EStr str -> prPrec i 4 (concatD [prt 0 str])
   EInt n -> prPrec i 4 (concatD [prt 0 n])
   EDouble d -> prPrec i 4 (concatD [prt 0 d])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Type where
  prt i e = case e of
   TInt  -> prPrec i 0 (concatD [doc (showString "int")])
   TDouble  -> prPrec i 0 (concatD [doc (showString "double")])



