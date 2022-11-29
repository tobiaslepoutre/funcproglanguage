-- Codé par Tobias LEPOUTRE (20177637) 
-- et Loïc Daudé Mondet (20243814)
-- le 27 octobre 2022.

-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use putStr" #-}

-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Évaluateur
-- - Pretty printer

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------
import qualified Data.Text as T
import Text.ParserCombinators.Parsec -- Librairie d'analyse syntaxique.
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
--                     (Scons (Scons (Ssym "-")
--                                   (Scons (Snum 68)
--                                          (Scons (Snum 32) Snil)))
--                            (Scons (Snum 5) Snil)))
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
                (pChar '\n' <|> eof); return ()
              }

-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

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
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire L(ambda)exp(ression)                     --
---------------------------------------------------------------------------

type Var = String

data Lexp = Lnum Int            -- Constante entière.
          | Lref Var            -- Référence à une variable.
          | Llambda Var Lexp    -- Fonction anonyme prenant un argument.
          | Lcall Lexp Lexp     -- Appel de fonction, avec un argument.
          | Lnil                -- Constructeur de liste vide.
          | Ladd Lexp Lexp      -- Constructeur de liste.
          | Lmatch Lexp Var Var Lexp Lexp -- Expression conditionelle.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lfix [(Var, Lexp)] Lexp
          deriving (Show, Eq)

------------------------------------------------------------------------
--- Fonction auxiliaires de s2l: ---

{- AUX0 (ou lireExp): Tentative de fonction auxilaire ayant pour objectif 
de généraliser "list" pour n'importe quel nombre d'arguments (fonction 
actuellement limité à 3 arguments).
lireExp (Scons x y)
    | y == Snil = e1
    | x == (Snum _) = Scons (Ssym "list") x (lireExp (Scons x'  y'))
    | otherwise = (Scons x y)
    where y = (Scons x' y'
-}
-- AUX0 avait pour objectif de fonctionner dans la fonction F0 et aurait 
-- permis de convertir un nombre quelconque d'arguement de "list" en son 
-- équivalent en "add" imbriqué tel que: 
-- (list 1 2 3 4 ...) -> (add 1 (add 2 (add 3 (add 4 ... )))

{- F0:
s2l (Scons (Ssym "list") (Scons n1 (lireExp (Scons x y)))) = 
    Ladd (s2l n1) (Ladd (s2l n2) (s2l ns))
-}

{- S0:  transforme les Scons en Lcall (Sexp -> Lexp)
s2l (Scons n1 (Scons n2 Snil)) =
    case s2l n1 of
        Lref a ->  Lcall (Lref a) (s2l n2)
        Lcall a b -> Lcall (Lcall a b) (s2l n2)
        _ -> Ladd (s2l n1) (s2l n2)
-}

-----------------------------------------------------------------------
-- Première passe simple qui analyse un Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym "nil") = Lnil
s2l (Ssym s) = Lref s
-- ¡¡ COMPLETER !!

-- S5: transforme les "add" de Sexp en Lexp
s2l (Scons (Ssym "add") (Scons x (Scons y Snil))) = Ladd (s2l x) (s2l y)

-- S4: transforme les "list" de 1, 2 et 3 éléments en Sexp, en leur équivalent 
-- "add" en Lexp à défaut de ne pas avoir réussi le cas général 
-- (voir les fonctions AUX0 et F0).
s2l (Scons (Ssym "list") (Scons x Snil)) = Ladd (s2l x) Lnil
s2l (Scons (Ssym "list") (Scons x (Scons y Snil))) = Ladd (s2l x) (s2l y)
s2l (Scons (Ssym "list") (Scons n1 (Scons n2 (Scons ns Snil)))) = 
    Ladd (s2l n1) (Ladd (s2l n2) (s2l ns))

-- S3: repère les 3 cas "(op var  num)", "(op num  var)" et "(op var var)" 
-- où op = *; + ; -; / , var = une variable Ssym et num =  un nombre Snum. 
s2l (Scons (Ssym op) (Scons (Ssym var) (Scons (Snum n1) Snil))) = 
    s2l (Ssym (showSexp (Scons (Ssym op) 
        (Scons (Ssym var) (Scons (Snum n1) Snil)))))
s2l (Scons (Ssym op) (Scons (Snum n1) (Scons (Ssym var) Snil))) = 
    s2l (Ssym (showSexp (Scons (Ssym op) 
        (Scons (Snum n1) (Scons (Ssym var) Snil)))))
s2l (Scons (Ssym op) (Scons (Ssym var1) (Scons (Ssym var2) Snil))) = 
    s2l (Ssym (showSexp (Scons (Ssym op) 
        (Scons (Ssym var1) (Scons (Ssym var2) Snil)))))

-- S2: traite le cas particulier des "fn"
s2l (Scons (Ssym "fn") (Scons (Scons (Ssym n1) Snil) (Scons n2 Snil))) = 
    Llambda n1 (s2l n2)

-- S1-1:  transforme les Scons en Lcall (Sexp -> Lexp)
s2l (Scons n1 (Scons n2 Snil)) = Lcall (s2l n1) (s2l n2)
-- S1-2 (généralisation): permet de lire récursivement les potentiels valeurs 
-- imbriquées de n1 et n2 en permettant à n1 d'être lu de la meme façon 
-- que n2 dans S1-1.
s2l (Scons s (Scons n1 n2)) = 
    s2l (Scons (Scons s (Scons n1 Snil)) n2)

-- EXTRA: nous informe sur les premieres erreur dans s2l: très utile 
-- dans les premières étapes de raisonnement.
s2l se = error ("Malformed Sexp: " ++ (showSexp se))

---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

-- Type des valeurs manipulée à l'exécution.
data Value = Vnum Int
           | Vnil
           | Vcons Value Value
           | Vfun (Value -> Value)

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec _ Vnil = showString "[]"
    showsPrec p (Vcons v1 v2) =
        let showTail Vnil = showChar ']'
            showTail (Vcons v1' v2') =
                showChar ' ' . showsPrec p v1' . showTail v2'
            showTail v = showString " . " . showsPrec p v . showChar ']'
        in showChar '[' . showsPrec p v1 . showTail v2
    showsPrec _ _ = showString "<function>"

type Env = [(Var, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: Env
env0 = [("+", Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x + y)))),
        ("*", Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x * y)))),
        ("/", Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x `div` y)))),
        ("-", Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x - y))))]

---------------------------------------------------------------------------
-- Représentation intermédiaire Dexp                                     --
---------------------------------------------------------------------------

-- Dexp est similaire à Lexp sauf que les variables sont représentées non
-- pas par des chaînes de caractères mais par des "Indexes de de Bruijn",
-- c'est à dire par leur distance dans l'environnment: la variable la plus
-- récemment déclarée a index 0, l'antérieure 1, etc...
--
-- I.e. Llambda "x" (Llambda "y" (Ladd (Lref "x") (Lref "y")))
-- se traduit par Dlambda (Dlambda (Dadd (Dref 1) (Dref 0)))

type Idx = Int

data Dexp = Dnum Int            -- Constante entière.
          | Dref Idx            -- Référence à une variable.
          | Dlambda Dexp        -- Fonction anonyme prenant un argument.
          | Dcall Dexp Dexp     -- Appel de fonction, avec un argument.
          | Dnil                -- Constructeur de liste vide.
          | Dadd Dexp Dexp      -- Constructeur de liste.
          | Dmatch Dexp Dexp Dexp -- Expression conditionelle.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Dfix [Dexp] Dexp
          deriving (Show, Eq)

------------------------------------------------------------------------
--- Fonction auxiliaires de l2d: ---

-- caract: transforme la variable de String -> Char pour replace.
caract (x:xs) = x

{- replace: remplace les variables de la fonction non-implémenté par 
ses valeurs correspondantes (inspiré de: https://stackoverflow.com
/questions/19545253/haskell-replace-characters-in-string). -}
replace var n1 = map (\c -> if c == (caract var) then (caract n1) else c)

-- lireS: conserve uniquement le String de Lref (Lexp->String).
lireS (Lref s) = s

-- lireN: conserve uniquement le Int de Lnum (Lexp->Int).
lireN (Lnum n) = n
  
------------------------------------------------------------------------

-- Le premier argument contient la liste des variables du contexte.
l2d :: [Var] -> Lexp -> Dexp
l2d _ (Lnum n) = Dnum n
-- ¡¡ COMPLETER !!

-- L0: fonction triviale (Lexp -> Dexp pour le )
l2d _ Lnil = Dnil

-- L1: reconnaitre le signe (+, *, / ou -) sachant 
-- que xs est la liste des variables du contexte:
l2d (x:xs) (Lref s)                      -- pour tout x de [+,-,/,*]:
  | s == x = Dref 0                      -- si s == x, l2d xs (Lref s) = Dref 0
  | otherwise = case l2d xs (Lref s) of  -- sinon, l2d xs (Lref s) =
    (Dref idx) -> Dref (idx + 1)         -- Dref 1->(*); 2->(/) ou 3->(-)

-- L2: taitement des cas particuliers:
    -- cas1 (des Ladd): comprenant à la fois "list" et "add".
    -- cas2 (fonctions imbriquées): utilise replace pour faire 
    -- l'implémentation des valeurs dans les fonctions.
    -- cas3 (fonction simple): tout comme le cas2, on utilise replace 
    -- pour l'affectation.
    -- cas 4 (Lcall générique): utilisé, en particulier, pour les 
    -- opérations arithmétiques.
l2d xs exp = case exp of 
    (Ladd x y) -> Dadd (l2d xs x) (l2d xs y)
    (Lcall (Lcall (Llambda var1 (Llambda var2 exp)) n1) n2) -> 
        l2d xs (lexpOf (replace var2 (show (lireN n2)) 
            (replace var1 (show (lireN n1)) (lireS exp))))
    (Lcall (Llambda var exp) n1) -> 
        l2d xs (lexpOf (replace var (show (lireN n1)) (lireS exp)))
    (Lcall (Lcall var lexp1) lexp2) ->  
        Dcall (Dcall (l2d xs var) (l2d xs lexp1)) (l2d xs lexp2)

----------------------------------------------------------------------
{- Tentatives de fonctions réalisées sur "let" et "match":

l2d xs (Llambda var lexp) =
    let env = (var : xs) in Dlambda (l2d env lexp) 

l2d xs (Lmatch lexp1 var1 var2 lexp2 lexp3) = 
    let env = (var1:xs)
        env2 = (var2:env)
        in Dmatch (l2d env2 lexp1) (l2d env2 lexp2) (l2d env2 lexp3) 

l2d xs (Lfix varexplist lexp) = 
    let varlist = map fst varexplist
        env = varlist ++ xs
        lexplist = map snd varexplist
    in 
        let dexplist = map (l2d env) lexplist 
        in Dfix dexplist (l2d env lexp) 
-}
----------------------------------------------------------------------
                                            



---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

-- Le premier argument contient la liste des valeurs des variables du
-- contexte, dans le même ordre que ces variables ont été passées à `l2d`.
eval :: [Value] -> Dexp -> Value
eval _ (Dnum n) = Vnum n
-- ¡¡ COMPLETER !!

eval _ (Dnil) = Vnil

-- Eval 1: fonction récursive qui fait correspondre l'index idx et 
-- la <fonction> associé dans env0.
eval (x:xs) (Dref idx) 
  | idx == 0  = x
  | otherwise = eval xs (Dref (idx - 1))

-- Eval 2: appels récursif de (eval xs e1) de manière à obtenir l'appel
-- de <fonction> -> f (appel recursif de e1 + affectation) et que
-- l'ensemble des Vnum imbriqué dans e2 -> (eval xs e2), de façon
-- à obtenir un résultat numérique.
eval xs (Dcall e1 e2) = let (Vfun f) = eval xs e1 in f (eval xs e2)

--Eval 3: 
eval xs (Dadd x y) = Vcons (eval xs x) (eval xs y)


---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = eval (map snd env0) . l2d (map fst env0) . s2l

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

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

dexpOf :: String -> Dexp
dexpOf = l2d (map fst env0) . s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf
