module Mannager where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import System.Random
import System.IO.Unsafe

-- Funcion para depurar el sistema creador a partir de un archivo de configuración
system_depurator :: System -> System
system_depurator (Sys act attr depAttr skl genThrows action_thw conModif)
                 = (DepSys (throwLinker act (throwTaker genThrows) action_thw) attr (reverse depAttr) skl conModif genThrows)

throwTaker :: ThrowsGen -> Throws
throwTaker (TrwGen t _) = t

throwLinker :: [Action] -> Throws -> [(Name,Throws)] -> [Action]
throwLinker (x:xs) thr [] = (throw_replace x thr):(throwLinker xs thr [])
throwLinker [] _ _        = []
throwLinker (x:xs) thr ys = let (z,ws) = lookAndReplace x thr ys in
                             z:throwLinker xs thr ws

lookAndReplace :: Action -> Throws -> [(Name,Throws)] -> (Action,[(Name,Throws)])
lookAndReplace act thr [] = (throw_replace act thr,[])
lookAndReplace a@(UAct name expr dice _) thr ((n,t):ys) = if n == name then (throw_replace a t,ys)
                                                        else let (z,w) = lookAndReplace a thr ys
                                                             in (z,(n,t):w)

throw_replace :: Action -> Throws -> Action
throw_replace (UAct name expr dice _) thr = (UAct name expr dice (Just thr))

-- Funciones para la creacion de personajes
character_creation :: Name -> Env -> [DepAttribute] -> [Skills] -> Character
character_creation name xs depAttr skl
                 = let ws = skillCalculator xs skl
                       ys = depAttrCalculator ws depAttr
                   in (PC name ys [])

depAttrCalculator :: Env -> [DepAttribute] -> Env
depAttrCalculator xs [] = xs
depAttrCalculator xs (y:ys) = case y of
                                DepAtr name expr -> case (runStateError (evalComm (Let name expr)) xs) of
                                                       Nothing -> error "Division por cero"
                                                       Just a -> depAttrCalculator (snd a) ys

skillCalculator :: Env -> [Skills] -> Env
skillCalculator s [] = s
skillCalculator s (y:ys) = case y of
                               Skill name expr  -> case (runStateError (evalComm (Let name expr)) s) of
                                                       Nothing -> error "Division por cero"
                                                       Just a -> skillCalculator (snd a) ys


-- Sistema de tirada de dados
dices_thrower :: Action -> Env -> ([Integer],[Integer],Integer)
dices_thrower (UAct _ intExp dice (Just thr)) s = case (runStateError (evalIntExp intExp) s) of
                                                      Nothing -> error "Division por cero"
                                                      Just a -> roll (diceToNum dice) thr (fst a)

skilatr_thrower :: Character -> Integer -> ThrowsGen -> ([Integer],[Integer],Integer)
skilatr_thrower (PC _ xs _) index (TrwGen thr dice) = roll (diceToNum dice) thr (snd (nth index xs))

roll :: Integer -> Throws -> Integer -> ([Integer],[Integer],Integer)
roll dice (Pool) num = roll dice (Num num) 0
roll dice thr num = case thr of
                      Num 1 -> let rl = aleatory_dice dice in
                               ([rl],[rl+num],rl+num)
                      Num n -> let (d,r,t) = roll dice (Num (n-1)) num
                                   rl = aleatory_dice dice in
                               (rl:d,(rl+num):r,t+rl+num)

aleatory_dice :: Integer -> Integer
aleatory_dice dice = unsafePerformIO (getStdRandom (randomR (1, dice)))

conModCheck :: Character -> [Con_Modifiers] -> Character
conModCheck p [] = p
conModCheck p@(PC name s st) ((ConMod n boolExp (All e ex)):ys) = case (runStateError (evalBoolExp boolExp) s) of
                                                                     Nothing -> error "Division por cero"
                                                                     Just a -> if fst a && (not (isIn n st)) then let s' = modifiersExecute s (commGenerator s ex e)
                                                                                                                 in conModCheck (PC name s' (n:st)) ys
                                                                                                            else conModCheck p ys
conModCheck p@(PC name s st) ((ConMod n boolExp (Coms xs)):ys) = case (runStateError (evalBoolExp boolExp) s) of
                                                                    Nothing -> error "Division por cero"
                                                                    Just a -> if fst a && (not (isIn n st)) then let s' = modifiersExecute s xs
                                                                                                                in conModCheck (PC name s' (n:st)) ys
                                                                                                           else conModCheck p ys

commGenerator :: Env -> [String] -> Comm -> [Comm]
commGenerator [] ys c = []
commGenerator ((x,_):xs) ys c = if (isIn x ys) then (commGenerator xs ys c) else  (replaceAllComm x c):(commGenerator xs ys c)

isIn :: String -> [String] -> Bool
isIn x [] = False
isIn x (y:ys) = (x == y) || (isIn x ys)

replaceAllComm :: Variable -> Comm -> Comm
replaceAllComm v (Let x e) = if x == "All" then (Let v (replaceAllInt v e)) else (Let x e)

replaceAllInt :: Variable -> IntExp -> IntExp
replaceAllInt v (UMinus x) = replaceAllInt v x
replaceAllInt v (Plus x y) = (Plus (replaceAllInt v x) (replaceAllInt v y))
replaceAllInt v (Minus x y) = (Minus (replaceAllInt v x) (replaceAllInt v y))
replaceAllInt v (Times x y) = (Times (replaceAllInt v x) (replaceAllInt v y))
replaceAllInt v (Div x y) = (Div (replaceAllInt v x) (replaceAllInt v y))
replaceAllInt v (Qmark b x y) = (Qmark (replaceAllbool v b) (replaceAllInt v x) (replaceAllInt v y))
replaceAllInt v (Const x) = (Const x)
replaceAllInt v (Var x) = if x == "All" then (Var v) else (Var x)

replaceAllbool :: Variable -> BoolExp -> BoolExp
replaceAllbool v (BTrue) = BTrue
replaceAllbool v (BFalse) = BFalse
replaceAllbool v (Eq x y) = (Eq (replaceAllInt v x) (replaceAllInt v y))
replaceAllbool v (Lt x y) = (Lt (replaceAllInt v x) (replaceAllInt v y))
replaceAllbool v (Gt x y) = (Gt (replaceAllInt v x) (replaceAllInt v y))
replaceAllbool v (And x y) = (And (replaceAllbool v x) (replaceAllbool v y))
replaceAllbool v (Or x y) = (Or (replaceAllbool v x) (replaceAllbool v y))
replaceAllbool v (Not x) = (Not (replaceAllbool v x))

modifiersExecute :: Env -> [Comm] -> Env
modifiersExecute s [] = s
modifiersExecute s (x:xs) = case (runStateError (evalComm x) s) of
                             Nothing -> error "Division por cero"
                             Just s' -> modifiersExecute (snd s') xs

modifiers :: System -> [Con_Modifiers]
modifiers (DepSys _ _ _ _ xs _) = xs

characterListModify :: System -> [Character] -> Integer -> Integer -> Integer -> [Character]
characterListModify sys (x:xs) 0 newNum n = let char = characterModify x newNum n
                                            in (conModCheck char (modifiers sys)):xs
characterListModify sys (x:xs) i newNum n = x:(characterListModify sys xs (i-1) newNum n)

characterModify :: Character -> Integer -> Integer -> Character
characterModify (PC name xs st) newNum n = (PC name (envModify xs newNum n) st)

envModify :: Env -> Integer -> Integer -> Env
envModify ((name,num):xs) newNum 0 = (name,newNum):xs
envModify ((name,num):xs) newNum n = (name,num):(envModify xs newNum (n-1))

nth :: Integer -> [a] -> a
nth _ [x] = x
nth 0 (x:xs) = x
nth n (x:xs) = nth (n-1) xs

length' :: [a] -> Integer
length' [] = 0
length' (x:xs) = 1+length' xs

takeOut :: Integer -> [a] -> [a]
takeOut _ [] = []
takeOut 1 (x:xs) = xs
takeOut n (x:xs) = x:(takeOut (n-1) xs)

diceToNum :: Dice -> Integer
diceToNum (D100) = 100
diceToNum (D20)  = 20
diceToNum (D12)  = 12
diceToNum (D10)  = 10
diceToNum (D8)   = 8
diceToNum (D6)   = 6
diceToNum (D4)   = 4
diceToNum (D3)   = 3

-- Estados
type Env = [(Variable,Integer)]

-- Estado nulo
initState :: Env
initState = []

-- Mónada estado
newtype StateError a = StateError { runStateError :: Env -> Maybe (a, Env) }

-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Integer
    -- Cambia el valor de una variable
    update :: Variable -> Integer -> m ()

-- Para calmar al GHC
instance Functor StateError where
    fmap = liftM

instance Applicative StateError where
    pure   = return
    (<*>)  = ap


-- Clase para representar mónadas que lanzan errores
class Monad m => MonadError m where
    -- Lanza un error
    throw :: m a

instance Monad StateError where
    return x = StateError (\s -> Just (x,s) )
    m >>= f = StateError (\s -> case (runStateError m s) of
                                    Nothing -> Nothing
                                    Just (v,s') -> ( runStateError (f v) s') )

instance MonadState StateError where
    lookfor v = StateError (\s -> Just (lookfor' v s, s))
                where lookfor' v ((u, j):ss) | v == u = j
                                             | v /= u = lookfor' v ss
    update v i = StateError (\s -> Just ((), update' v i s))
                 where update' v i [] = [(v, i)]
                       update' v i ((u, _):ss) | v == u = (v, i):ss
                       update' v i ((u, j):ss) | v /= u = (u, j):(update' v i ss)


instance MonadError StateError where
   throw = StateError (\_-> Nothing)

-- Evalua un comando
evalComm :: (MonadState m, MonadError m) => Comm -> m ()
evalComm (Let v n)      = evalIntExp n >>=  (\m-> update v m)

-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: (MonadState m, MonadError m) => IntExp -> m Integer
evalIntExp (Const n)   = return n
evalIntExp (Var v)     = lookfor v
evalIntExp (UMinus n)  = evalIntExp n >>= (\m->return (-m))
evalIntExp (Plus n m)  = evalIntExp n >>= (\a-> evalIntExp m >>= (\b -> return (a+b)) )
evalIntExp (Minus n m) = evalIntExp n >>= (\a-> evalIntExp m >>= (\b -> return (a-b)) )
evalIntExp (Times n m) = evalIntExp n >>= (\a-> evalIntExp m >>= (\b -> return (a*b)) )
evalIntExp (Qmark b i j) = evalBoolExp b >>= (\a -> evalIntExp i >>= (\b -> evalIntExp j >>= (\c -> if a then return b else return c)))
evalIntExp (Div n m)   = evalIntExp n >>= (\a-> evalIntExp m >>= (\b -> if b == 0 then throw else return (div a b)) )

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: (MonadState m, MonadError m) => BoolExp -> m Bool
evalBoolExp BTrue       = return True
evalBoolExp BFalse      = return False
evalBoolExp (Eq n m)    = evalIntExp n >>=(\a-> evalIntExp m >>= (\b -> return (a==b)) )
evalBoolExp (Gt n m)    = evalIntExp n >>=(\a-> evalIntExp m >>= (\b -> return (a>b)) )
evalBoolExp (Lt n m)    = evalIntExp n >>=(\a-> evalIntExp m >>= (\b -> return (a<b)) )
evalBoolExp (Not b)     = evalBoolExp b >>= (\a-> return (not a))
evalBoolExp (And b1 b2) = evalBoolExp b1 >>= (\a-> evalBoolExp b2 >>= (\b -> return (a && b)) )
evalBoolExp (Or b1 b2)  = evalBoolExp b1 >>= (\a-> evalBoolExp b2 >>= (\b -> return (a || b)) )
