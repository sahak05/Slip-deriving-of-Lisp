-- TP-2  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}

-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Évaluateur
-- - Pretty printer

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Libraire d'analyse syntaxique (et lexicale).
import Data.Char        -- Conversion de Chars de/vers Int et autres
-- import Numeric       -- Pour la fonction showInt
import System.IO        -- Pour stdout, hPutStr
-- import Data.Maybe    -- Pour isJust and fromJust

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
-- Génère automatiquement un pretty-printer et une fonction de
-- comparaison structurelle.
           deriving (Show, Eq)

-- Exemples:
-- (+ 2 3) == (+ . (2 . (3 . ())))
--         ==> Scons (Ssym "+")
--                   (Scons (Snum 2)
--                          (Scons (Snum 3) Snil))
--
-- (/ (* (- 68 32) 5) 9)
--     ==>
-- Scons (Ssym "/")
--       (Scons (Scons (Ssym "*")
--                     (Scons   (Scons   
--                           (Ssym "-")      
--                         (Scons   
--                               (Snum 68)    
--                                 (Scons    
--                          (Snum 32) Snil) 
--                                )
--                          )
--             (Scons (Snum 5) Snil)   
--               )
--                       )
--              (Scons (Snum 9) Snil))

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                pChar '\n'; return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment); return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp
pQuote = do { pChar '\''; pSpaces; e <- pSexp;
              return (Scons (Ssym "quote") (Scons e Snil)) }

-- Une liste est de la forme:  ( {e} [. e] )
pList :: Parser Sexp
pList  = do { pChar '('; pSpaces; pTail }
pTail :: Parser Sexp
pTail  = do { pChar ')'; return Snil }
     <|> do { pChar '.'; pSpaces; e <- pSexp; pSpaces;
              pChar ')' <|> error ("Missing ')' after: " ++ show e);
              return e }
     <|> do { e <- pSexp; pSpaces; es <- pTail; return (Scons e es) }

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pSpaces;
                pList <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _p s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) =
    let showTail Snil = showChar ')'
        showTail (Scons e1' e2') =
            showChar ' ' . showSexp' e1' . showTail e2'
        showTail e = showString " . " . showSexp' e . showChar ')'
    in showChar '(' . showSexp' e1 . showTail e2

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de Hugs).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire L(ambda)exp(ression)                     --
---------------------------------------------------------------------------

type Var = String
type Tag = String
type Pat = Maybe (Tag, [Var])
data BindingType = Lexical | Dynamic
                   deriving (Show, Eq)
    
data Lexp = Lnum Int            -- Constante entière.
          | Lvar Var            -- Référence à une variable.
          | Lfn Var Lexp        -- Fonction anonyme prenant un argument.
          | Lpipe Lexp Lexp     -- Appel de fonction, avec un argument.
          | Lcons Tag [Lexp]    -- Constructeur de structure de donnees.
          | Lcase Lexp [(Pat, Lexp)] -- Expression conditionelle.
          | Llet BindingType Var Lexp Lexp -- Déclaration de variable locale
          deriving (Show, Eq)

-- Première passe simple qui analyse un Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s
s2l (Scons (Snum a)Snil) = s2l (Snum a)
s2l (Scons (Ssym a)Snil) = s2l (Ssym a)

-- cas du (e0 e1 e2 ... en) ⇐⇒ (..((e0 e1) e2) ... en)
s2l(Scons (x1) (Scons (Scons (x2) (Scons (Ssym c) Snil)) Snil)) 
              = Lpipe (s2l(x1)) (Lpipe (s2l(x2)) (s2l(Ssym c))) 

s2l (Scons (Scons x1 exp1) Snil) = s2l (Scons x1 exp1)

--cas ou le sucre syntaxique nest pas 
s2l(Scons (Snum a) 
           (Scons (Scons (Ssym "lambda")
                  (Scons (Scons (Ssym b) Snil)
                  (Scons (Ssym c) 
            Snil))) 
    Snil)) = Lfn c (Lfn b (s2l (Snum a)))

--cas avec le sucre syntaxique
s2l (Scons x1 (Scons (Scons x2 
                  (Scons (Scons (Ssym "lambda")
                         (Scons (Scons (Ssym a1) Snil)
                  (Scons (Scons (Ssym "lambda")
                          (Scons (Scons (Ssym a2) Snil) 
                  (Scons (Scons (Ssym a3)(Scons (Scons (Ssym a4)
              (Scons (Ssym b) Snil)) Snil)) 
    Snil))) 
    Snil))) Snil)) Snil)) = Lfn a1 (Lfn a2 (Lpipe (s2l x1) (Lpipe (s2l x2) 
    (s2l(Scons (Ssym a3) (Scons (Scons ((Ssym a4))
                                      (Scons (Ssym b) Snil)) Snil))))))

--cas de tag avec cons 
s2l( Scons (Ssym "cons") (Scons (Ssym tag) e)) = 
                                    Lcons tag mytabFct
                                    where mytabFct = list e
                                    --mettre tout les expressions suivant 
                                    --un tag dans une liste
                                          list :: Sexp -> [Lexp] 
                                          list (Snum a) = [s2l (Snum a)]
                                          list (Ssym a) = [s2l (Ssym a)]
                                          list (Snil) = []
                                          list (Scons a b) = s2l(a):(list b)  

--cas du if ayant du sucre syntaxique soo remplacement avec case 
s2l (Scons (Ssym "if") (Scons e1 (Scons e4 (Scons e5 Snil)))) = 
  s2l (Scons (Ssym "case")
     (Scons e1(Scons (Scons (Scons (Ssym "true") Snil) (Scons e4 Snil))
     (Scons(Scons (Scons (Ssym "false") Snil) (Scons e5 Snil))Snil ))))


--cas du case 
-- Lcase Lexp [(Pat, Lexp)] sachant que le Pat c'est 
--Maybe (Tag, [Var])
s2l ( Scons (Ssym "case")(Scons e1 e2)) = 
      Lcase (s2l e1) condition
      where condition = tri e2
            tri :: Sexp -> [(Pat, Lexp)]
            tri Snil = []
            tri (Scons (Scons (Ssym "_") b) x) = (Nothing,(s2l b)):(tri x)
            tri (Scons (Scons (Scons (Ssym a) b) c) x) =
                    ((Just(a, (tableau b))),(s2l c)):(tri x)
            --tri (Scons a b) = (tri a)++(tri b)
            tri _ = error "here we have too"--on va jamais si Sexp bien
            tableau :: Sexp -> [Var]
            tableau Snil = []
            tableau (Scons (Ssym a) b) = a:(tableau b)
            tableau _ = error "we have an error here"

--cas du slet
s2l ( Scons (Ssym "slet") (Scons x expressionE)) = 
                            
    let temp = x
      in case temp of 
      (Scons (Scons (Ssym e1) exp1) Snil) ->Llet Lexical e1 (s2l exp1)
        (s2l expressionE)
      (Scons y1 y2) -> s2l (Scons (Ssym "slet") (Scons (Scons y1 Snil)
        (Scons (Ssym "slet")(Scons y2 expressionE))))
      _ -> error "No match"


--cas du dlet
s2l ( Scons (Ssym "dlet") (Scons x expressionE)) = 
                            
      let temp = x
        in case temp of 
        (Scons (Scons (Ssym e1) exp1) Snil)
                   ->Llet Dynamic e1 (s2l exp1) (s2l expressionE)
        _ -> error "No match"
-- ¡¡ COMPLETER !!
s2l se = error ("Malformed Sexp: " ++ (showSexp se))

---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

type Arity = Int

-- Type des valeurs manipulée à l'exécution.
data Value = Vnum Int
           | Vcons Tag [Value]
           | Vfn (Env -> Value -> Value)

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vcons tag vs) =
        let showTail [] = showChar ']'
            showTail (v : vs') =
                showChar ' ' . showsPrec p v . showTail vs'
        in showChar '[' . showString tag . showTail vs
    showsPrec _ (Vfn _)
        = showString ("<function>")

type Env = [(Var, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: Env
env0 = let false = Vcons "false" []
           true = Vcons "true" []
           mkbop (name, op) =
               (name, Vfn (\ _ (Vnum x)
                           -> Vfn (\ _ (Vnum y)
                                   -> Vnum (x `op` y))))
           mkcmp (name, op) =
               (name, Vfn (\ _ (Vnum x)
                           -> Vfn (\ _ (Vnum y)
                                   -> if x `op` y then true else false)))
       in [("false", false),
           ("true", true)]
          ++ map mkbop
              [("+", (+)),
               ("*", (*)),
               ("/", div),
               ("-", (-))]
          ++ map mkcmp
              [("<=", (<=)),
               ("<", (<)),
               (">=", (>=)),
               (">", (>)),
               ("=", (==))]


---------------------------------------------------------------------------
-- Espaces fonctions auxillaires                                         --
---------------------------------------------------------------------------

reconnai :: (Maybe (Tag, [Var]), Lexp) -> (Tag, [Var])
reconnai a = let cst = a 
              in case cst of
               ((Just y), _) -> y
               _ -> error "not a just tuple"


foundinEnv :: Var ->[(Var, Value)]-> Value
--je veux pas avoir le cas ou ca n'existe pas encore mais on peux mettre ca 
foundinEnv a [] = error ("votre variable "++ show a ++" est inexistante !!")
foundinEnv  a (x:xs) = if (a == (fst x)) then snd x
                            else foundinEnv a xs 

--Ajoute (variable, value) a l'environnement en verifiant si un tuple
--commencant par la variable ny est pas
--source 
--stackoverflow.com/questions/29135331/removing-tuples-from-list-haskell
addEnv :: Var -> Value -> Env -> Env 
addEnv x xv esp = [c | c<- esp, fst c /= x ] ++[(x,xv)]

--filtre dans le tableau venant avec Lcase pour retourner le Lexp a evaluer
filtrage :: Value ->[(Maybe (Tag, [Var]), Lexp)] -> Lexp
filtrage _ [] = error ("we got an error in the filtrage fct")
filtrage a ((Nothing,x):xs) = 
                let temp = Nothing 
                 in case temp of
                 Nothing -> x
                 _ -> filtrage a xs

filtrage (Vcons a b) (x:xs) = if 
                          ((a) == fst(reconnai x)) then snd x 
                            else filtrage (Vcons a b) xs
filtrage _ _ = error "we can't have that error" 

--filtre sur tableau en utilisant des Var en retournant le tuple 
filtrage2 :: Var ->[(Maybe (Tag, [Var]), Lexp)] -> (Maybe (Tag, [Var]), Lexp)
filtrage2 _ [] = error ("we got an error in the filtrage fct")
filtrage2 a ((Nothing,x):xs) = 
                let temp = Nothing 
                 in case temp of
                 Nothing -> (Nothing,x)
                 _ -> filtrage2 a xs

filtrage2 a (x:xs) = if (a == fst(reconnai x)) then x else filtrage2 a xs
---------------------------------------------------------------------------
-- Fin Espaces donctions auxillaires                                     --
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

eval :: Env -> Env -> Lexp -> Value
eval _senv _denv (Lnum n) = Vnum n 
eval _senv _denv (Lvar x) = foundinEnv x _senv 
eval _senv _denv (Llet x  y z t) = 
            case x of
            Lexical -> eval (addEnv y (eval _senv _denv z) _senv) _denv t
            _ -> error"On a pas eu celui de Dynamic a temps"  

eval _senv _denv (Lpipe x1 (Lpipe x2 (Lvar x3)) ) = 
        let Vfn funct = eval _senv _denv (Lvar x3)  
            Vfn f = funct _senv (eval _senv _denv x2)
                in f _senv (eval _senv _denv x1)



eval _senv _denv (Lfn x (Lfn y (Lpipe z (Lpipe t u)))) = 
            let varIn = addEnv  x (eval _senv _denv z) _senv
                varIn1 = addEnv y (eval _senv _denv t) varIn
                in eval varIn1 _denv u
              

eval _senv _denv (Lfn c (Lfn b x)) = 
                   foundinEnv c (addEnv b (eval _senv _denv x) _senv)


eval _senv _denv (Lcons tag xl) = 
            Vcons tag xl' --on filtre tout les elemnts de la liste
              where  
                    xl' = filtre xl
                    filtre :: [Lexp] -> [Value]
                    filtre [] = []
                    filtre (x:xs) = (eval _senv _denv x):(filtre xs)

eval _senv _denv (Lcase (Lcons y exp') tab1) = 
             let trans = filtrage2 y tab1
               in case trans of
               (Nothing,x) -> eval _senv _denv x
               (Just(a,b),c) -> eval newEnv _denv c
                              where
                                  change = eval _senv _denv (Lcons y exp') 
                                  newEnv = (_senv++(concat1 (a,b) change))
                                  
                                  concat1 :: (Tag, [Var]) -> Value ->Env 
                                  concat1 (_,t0) (Vcons _ t1) = ens t0 t1
                                  concat1 _ _ = error "error"
                                  ens :: [Var]->[Value]->Env 
                                  ens (x:xs) (x1:x1s) = (x,x1):(ens xs x1s)
                                  ens _ _ = error "impossible" 

eval _senv _denv (Lcase e1 tab1) = 
                     eval _senv _denv (filtrage (eval _senv _denv e1) tab1)
-- ¡¡ COMPLETER !!
eval _ _ e = error ("Can't eval: " ++ show e)

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = eval env0 [] . s2l

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do s <- readFile filename
       (hPutStr stdout . show)
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
             in map evalSexp (sexps s))