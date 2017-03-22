module AST where

type Variable = String

-- Sinonimo de tipo para el nombre de los atributos
type Name = String

-- Representacion del sistema
data System = Sys [Action] Attribute [DepAttribute] [Skills] ThrowsGen [(Name,Throws)] [Con_Modifiers]
            | DepSys [Action] Attribute [DepAttribute] [Skills] [Con_Modifiers] ThrowsGen
 deriving Show

-- Representación de los atributos bajo determinada clase
data Attribute = Atr Name [Name] deriving Show

-- Representacion de los atributos dependientes
data DepAttribute = DepAtr Name IntExp deriving Show

-- Representacion de las acciones
data Action = UAct Name IntExp Dice (Maybe Throws)
 deriving Show

-- Representacion de las habilidades
data Skills = Skill Name IntExp deriving Show

-- Representacion del juego
data Game = Gm System [Character]

-- Representacion de los personajes
data Character = PC Name [(Name,Integer)] [String] deriving Show

-- Representacion de las tiradas
data Throws = Pool
            | Num Integer
 deriving Show

data ThrowsGen = TrwGen Throws Dice deriving Show

-- Representacion de las modificaciones condicionalas
data Con_Modifiers = ConMod Name BoolExp Modificator deriving Show

data Modificator = Coms [Comm] | All Comm [Variable] deriving Show

data Dice = D100
          | D20
          | D12
          | D10
          | D8
          | D6
          | D4
          | D3
 deriving Show

data IntExp = Const Integer
            | Var Name
            | UMinus IntExp
            | Plus IntExp IntExp
            | Minus IntExp IntExp
            | Times IntExp IntExp
            | Div IntExp IntExp
            | Qmark BoolExp IntExp IntExp
 deriving Show

-- Expresiones Booleanas
data BoolExp = BTrue
             | BFalse
             | Eq IntExp IntExp
             | Lt IntExp IntExp
             | Gt IntExp IntExp
             | And BoolExp BoolExp
             | Or BoolExp BoolExp
             | Not BoolExp
 deriving Show

data Comm = Skip
           | Let Variable IntExp
 deriving Show
