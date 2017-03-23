module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
                  whiteSpace lis
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart  = "/*"
                                  , commentEnd    = "*/"
                                  , commentLine   = "//"
                                  , reservedOpNames = ["+", "-", "*", "/","?",":"]
                                  , opLetter      = char '='
                                  , reservedNames = ["true","false","skip","if",
                                                     "then","else","end", "do",
                                                     "ATTRIBUTE","ALL","DEP_ATTRIBUTE","NON","CONDITION_MODIFIER","CONDITION_MODIFIER_ALL",
                                                     "UNIT_ACTION","DOUBLE_ACTION","D3",
                                                     "D4","D5","D6","D8","D10","D12",
                                                     "D20","D100","SKILLS","THROW_GENERAL","THROW_ACTION"]
                                  })

----------------------------------
--- Parser de archivos de configuracion de sistemas
-----------------------------------

num_parse :: String -> Integer
num_parse num = case parse (totParser (natural lis)) "" num of
                  Left err -> 0
                  Right a  -> a

system :: System -> Parser System
system t@(Sys act atr depatr skl mainTrhow action_thw conMod) = do try (do x <- attribute
                                                                           system (Sys act x depatr skl mainTrhow action_thw conMod))
                                                                <|> (do y <- action
                                                                        system (Sys (y:act) atr depatr skl mainTrhow action_thw conMod))
                                                                <|> (do z <- depattribute
                                                                        system (Sys act atr (z:depatr) skl mainTrhow action_thw conMod))
                                                                <|> (do w <- skills
                                                                        system (Sys act atr depatr w mainTrhow action_thw conMod))
                                                                <|> (do c <- conModifiers
                                                                        system (Sys act atr depatr skl mainTrhow action_thw (c:conMod)))
                                                                <|> (do g <- generalTrhow
                                                                        system (Sys act atr depatr skl g action_thw conMod))
                                                                <|> (do p <- actionTrhow
                                                                        system (Sys act atr depatr skl mainTrhow (p:action_thw) conMod))
                                                                <|> return t

loadCharacter :: Parser [Character]
loadCharacter = sepBy1 characterParser (symbol lis "+")

characterParser :: Parser Character
characterParser = do n <- identifier lis
                     xs <- sepBy1 estateParser (symbol lis ";")
                     reservedOp lis "CONDITION_MODIFIER"
                     ys <- sepBy (identifier lis) (symbol lis ";")
                     return (PC n xs ys)

estateParser :: Parser (Name,Integer)
estateParser = do n <- identifier lis
                  v <- natural lis
                  return (n,v)

conModifiers :: Parser Con_Modifiers
conModifiers  =   try (do reservedOp lis "CONDITION_MODIFIER"
                          v <- identifier lis
                          symbol lis "{"
                          c <- boolexp
                          symbol lis ";"
                          symbol lis "{"
                          xs <- sepBy1 comm (symbol lis ";")
                          symbol lis "}"
                          symbol lis "}"
                          return (ConMod v c (Coms xs)))
                  <|> (do reservedOp lis "CONDITION_MODIFIER_ALL"
                          v <- identifier lis
                          symbol lis "{"
                          b <- boolexp
                          symbol lis ";"
                          c <- comm
                          symbol lis ";"
                          symbol lis "{"
                          xs <- sepBy1 (identifier lis) (symbol lis ";")
                          symbol lis "}"
                          symbol lis "}"
                          return (ConMod v b (All c xs)))

skills :: Parser [Skills]
skills = do reservedOp lis "SKILLS"
            symbol lis "{"
            xs <- sepBy1 single_skill (symbol lis ";")
            symbol lis "}"
            return xs

single_skill :: Parser Skills
single_skill = do try (do v <- identifier lis
                          reservedOp lis "NON"
                          return (Skill v (Const 0)))
                  <|> (do v <- identifier lis
                          x <- intexp
                          return (Skill v x))

generalTrhow :: Parser ThrowsGen
generalTrhow = do try (do reservedOp lis "THROW_GENERAL"
                          reservedOp lis "POOL"
                          d <- dices
                          return (TrwGen Pool d))
                  <|> (do reservedOp lis "THROW_GENERAL"
                          n <- natural lis
                          d <- dices
                          return (TrwGen (Num n) d))

actionTrhow :: Parser (Name,Throws)
actionTrhow = do try (do reservedOp lis "THROW_ACTION"
                         v <- identifier lis
                         reservedOp lis "POOL"
                         return (v,Pool))
                 <|> (do reservedOp lis "THROW_ACTION"
                         v <- identifier lis
                         n <- natural lis
                         return (v,(Num n)))


depattribute :: Parser DepAttribute
depattribute = do reservedOp lis "DEP_ATTRIBUTE"
                  v <- identifier lis
                  symbol lis "{"
                  xs <- intexp
                  symbol lis "}"
                  return (DepAtr v xs)

attribute :: Parser Attribute
attribute = do reservedOp lis "ATTRIBUTE"
               v <- identifier lis
               symbol lis "{"
               xs <- sepBy1 (identifier lis) (symbol lis ";")
               symbol lis "}"
               return (Atr v xs)

action :: Parser Action
action = do reservedOp lis "UNIT_ACTION"
            d <- dices
            v <- identifier lis
            symbol lis "{"
            c <- intexp
            symbol lis "}"
            return (UAct v c d Nothing)

dices :: Parser Dice
dices = try (do reservedOp lis "D3"
                return D3)
        <|> (do reservedOp lis "D100"
                return D100)
        <|> (do reservedOp lis "D4"
                return D4)
        <|> (do reservedOp lis "D6"
                return D6)
        <|> (do reservedOp lis "D8"
                return D8)
        <|> (do reservedOp lis "D10"
                return D10)
        <|> (do reservedOp lis "D12"
                return D12)
        <|> (do reservedOp lis "D20"
                return D20)

----------------------------------
--- Parser de expressiones enteras
-----------------------------------

intexp :: Parser IntExp
intexp  = chainl1 term addop

addop :: Parser (IntExp -> IntExp -> IntExp)
addop  = try (do reservedOp lis "+"
                 return (Plus))
         <|> try (do reservedOp lis "-"
                     return (Minus))

term :: Parser IntExp
term  = chainl1 factor mulop

mulop :: Parser (IntExp -> IntExp -> IntExp)
mulop  = try (do reservedOp lis "*"
                 return (Times))
         <|> try (do reservedOp lis "/"
                     return (Div))

factor :: Parser IntExp
factor  = try (do n <- natural lis
                  return (Const n))
          <|> try (do v <- identifier lis
                      return (Var v))
          <|> try (do reservedOp lis "-"
                      f <- factor
                      return (UMinus f))
          <|> try (do parens lis intexp)
          <|> try (do c <- boolexp
                      reservedOp lis "?"
                      i <- intexp
                      reservedOp lis ":"
                      j <- intexp
                      return (Qmark c i j))

-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser BoolExp
boolexp = chainl1 orpar orop

andop :: Parser (BoolExp -> BoolExp -> BoolExp)
andop = do reservedOp lis "&"
           return (And)

orpar :: Parser BoolExp
orpar = chainl1 boolfac andop

orop :: Parser (BoolExp -> BoolExp -> BoolExp)
orop = do reservedOp lis "|"
          return (Or)

boolfac :: Parser BoolExp
boolfac = try (do reservedOp lis "true"
                  return (BTrue))
          <|> try (do reservedOp lis "false"
                      return (BFalse))
          <|> try (do reservedOp lis "~"
                      t <- boolfac
                      return (Not t))
          <|> do parens lis boolexp
          <|> try (do t <- intexp
                      reservedOp lis "="
                      t' <- intexp
                      return (Eq t t'))
          <|> try (do t <- intexp
                      reservedOp lis "<"
                      t' <- intexp
                      return (Lt t t'))
          <|> try (do t <- intexp
                      reservedOp lis ">"
                      t' <- intexp
                      return (Gt t t'))

comm       :: Parser Comm
comm = do v <- identifier lis
          reservedOp lis ":="
          i <- intexp
          return (Let v i)

------------------------------------
-- Función de parseo
------------------------------------
parseSystem :: Parser System
parseSystem = totParser (system (Sys [] (Atr "Vacio" []) [] [] (TrwGen (Num 1) D6) [] []))
