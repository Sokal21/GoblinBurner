module Interface where

import Parser
import Mannager
import AST
import Text.Parsec.String
import System.Process

skillTitle :: String
skillTitle = "------------------------------------------------\nChoose the skill class - enter 0 when you finish\n------------------------------------------------\n"

welcome :: String
welcome = "|------------------------------------------|\n|       Welcome to GoblinBurner v1.8       |\n|------------------------------------------|\n\n"

start :: IO ()
start = do fileStarter ""

fileStarter :: String -> IO ()
fileStarter er = do file <- putAndWait (er++"\n"++welcome++"Please enter your config-file name \n")
                    sys <- parseFromFile parseSystem ("files/"++file)
                    case sys of
                      Right a -> menu (Gm (system_depurator a) []) menu_title
                      Left er -> do fileStarter (show er)

menu :: Game -> [(String, (Game -> IO (Game,String)))] -> IO ()
menu game m   = do System.Process.system "clear"
                   cmd <- putAndWait (welcome++(menu_list m 1))
                   case (nth ((num_parse cmd) - 1) m) of
                       (_, f) -> do (game',err') <- f game
                                    if err' == "Exit" then return () else (menu game' m)

showCharacter :: [Character] -> IO ()
showCharacter chars = do c <- putAndWait ((character_list chars 1)++"Which character you want to see?\n")
                         showAtributes (nth ((num_parse c)-1) chars)

showAtributes :: Character -> IO ()
showAtributes (PC name env cond) = do putAndWait ("Name: "++name++"\n"++"Attributes: \n"++(listEnv env)++"\n"++"Conditions: \n"++(listCon cond)++"\nPress any key to continue \n")
                                      return ()
modifyCharacter :: System -> [Character] -> IO ([Character])
modifyCharacter sys chars = do c <- putAndWait ((character_list chars 1)++"Which character you want to modify?\n")
                               a <- putAndWait ("Which attribute you will modify?\n"++(skilatr_list (envFromCharacter ((nth ((num_parse c)-1)) chars)) 1))
                               n <- putAndWait ("Enter the new value\n")
                               return (characterListModify sys chars ((num_parse c)-1) (num_parse n) ((num_parse a)-1))

loadCharaterInterface :: IO ([Character])
loadCharaterInterface = do file <- putAndWait "Please enter your character file \n"
                           char <- parseFromFile (totParser (loadCharacter [])) ("files/"++file)
                           case char of
                              Right a -> return a
                              Left er -> return ([])

delete_character :: [Character] -> IO ([Character])
delete_character char = do m <- putAndWait ("Which character you want to delete?\n"++(character_list char 1))
                           return (takeOut (num_parse m) char)

saveCharacther :: [Character] -> IO ()
saveCharacther chars = do c <- putAndWait ("Which character do you want to save?\n"++(character_list chars 1))
                          case nth ((num_parse c)-1) chars of
                            (PC name xs ys) -> writeFile ("files/"++name) (name++"\n"++(listEnv xs)++"CONDITION_MODIFIER\n"++(listCon ys))

roll_skilatr :: System -> [Character] -> IO()
roll_skilatr sys chars = do c <- putAndWait ("Which character do you want to use?\n"++(character_list chars 1))
                            System.Process.system "clear"
                            a <- putAndWait ("Which attribute you will use?\n"++(skilatr_list (envFromCharacter ((nth ((num_parse c)-1)) chars)) 1))
                            trw <- return ((skilatr_thrower (nth ((num_parse c)-1) chars) ((num_parse a)-1)) (generalTrhowFromSystem sys))
                            putAndWait ((printRoll trw)++"Press any key to continue \n")
                            return ()

skilatr_list :: Env -> Integer -> String
skilatr_list [] n = "\n"
skilatr_list ((name,num):xs) n = ((show n)++" - "++name++" : "++(show num)++"\n")++skilatr_list xs (n+1)

menu_list :: [(String,a)] -> Integer -> String
menu_list [] n = ""
menu_list ((x,_):xs) n = ((show n)++" - "++x++"\n")++(menu_list xs (n+1))

generalTrhowFromSystem :: System -> ThrowsGen
generalTrhowFromSystem (DepSys _ _ _ _ _ throwsGen) = throwsGen

make_action :: System -> [Character] -> IO ()
make_action sys chars = do c <- putAndWait ("Which character do you want to use?\n"++(character_list chars 1))
                           a <- putAndWait ("Which action do you want to roll?\n"++(action_list (actionsFromSystem sys) 1))
                           trw <- return (dices_thrower (nth ((num_parse a)-1) (actionsFromSystem sys)) (envFromCharacter (nth ((num_parse c)-1) chars)))
                           putAndWait ((printRoll trw)++"Press any key to continue \n")
                           return ()

printRoll :: ([Integer],[Integer],Integer) -> String
printRoll (dice,dicePlus,total) =("The total of the roll is: "++(show total)++"\n")
                                  ++"The dice roll where:\n"
                                  ++((show dice)++"\n")
                                  ++"The dice plus de modifier:\n"
                                  ++((show dicePlus)++"\n")

action_list :: [Action] -> Integer -> String
action_list [] _ = "\n"
action_list ((UAct name _ _ _):xs) n = ((show n)++" - "++name++"\n")++action_list xs (n+1)

envFromCharacter :: Character -> Env
envFromCharacter (PC _ env _) = env

actionsFromSystem :: System -> [Action]
actionsFromSystem (DepSys act _ _ _ _ _) = act

character_list :: [Character] -> Integer -> String
character_list [] n = "\n"
character_list ((PC name _ _):xs) n = ((show n)++" - "++name++"\n")++character_list xs (n+1)

create_char :: System -> IO Character
create_char (DepSys _ attr depAttr skl _ _) = do name <- putAndWait "Enter your character name!\n"
                                                 case attr of
                                                   Atr _ list -> do env <- get_attributes list []
                                                                    (s,l) <- (skill_picker skl ([],[])) ""
                                                                    skills_modifier (character_creation name env depAttr s) l

skill_picker :: [Skills] -> ([Skills],[Name]) -> String -> IO ([Skills],[Name])
skill_picker skill (s,l) er  = do cmd <- putAndWait ((skillTitle)++(printSkillClass skill 1))
                                  if (num_parse cmd) == 0 then
                                   return (s,l)
                                  else if ((num_parse cmd) > (length' skill)) || ((num_parse cmd) < 0) then skill_picker skill (s,l) "Wrong index \n"
                                                                                                       else do putStr "Enter the name of the skill\n"
                                                                                                               name <- getLine
                                                                                                               case nth ((num_parse cmd)-1) skill of
                                                                                                                 (Skill _ intexp) -> skill_picker skill ((Skill name intexp):s,name:l) ""

printSkillClass :: [Skills] -> Integer -> String
printSkillClass [] _ = "\n"
printSkillClass ((Skill name _):xs) n = ((show n)++" - "++name++"\n")++printSkillClass xs (n+1)

get_attributes :: [Name] -> Env -> IO Env
get_attributes [] env     = do System.Process.system "clear"
                               return env
get_attributes (x:xs) env = do num <- putAndWait ("Enter the atribute modifier for "++x++"\n")
                               get_attributes xs ((x,num_parse num):env)

skills_modifier :: Character -> [Name] -> IO Character
skills_modifier char []     = do System.Process.system "clear"
                                 return char
skills_modifier (PC name env ys) (x:xs) = do num <- putAndWait ("Enter the skill modifier for "++x++"\n")
                                             skills_modifier (PC name (skill_update env x (num_parse num)) ys) xs

skill_update :: Env -> Name -> Integer -> Env
skill_update ((name,n):xs) skill num = if name == skill then (name,n+num):xs else (name,n):(skill_update xs skill num)

putAndWait :: String -> IO (String)
putAndWait s = do System.Process.system "clear"
                  putStr s
                  getLine

listEnv :: Env -> String
listEnv [] = []
listEnv [(n,num)] = (n++" "++(show num))
listEnv ((n,num):xs) = (n++" "++(show num)++";\n")++(listEnv xs)

listCon :: [String] -> String
listCon [] = []
listCon [x] = x++"\n"
listCon (x:xs) = x++";\n"++(listCon xs)

name_skill :: Skills -> Name
name_skill (Skill name _ ) = name

cmdNewChar :: Game -> IO (Game,String)
cmdNewChar g@(Gm system char) = do c <- create_char system
                                   return ((Gm system (c:char) ), "")

cmdRollAtr :: Game -> IO (Game,String)
cmdRollAtr g@(Gm system char) = do roll_skilatr system char
                                   return (g, "")

cmdListChar :: Game -> IO (Game,String)
cmdListChar g@(Gm system char) = do putAndWait ((character_list char 1)++"Press any key to continue \n")
                                    return (g, "")

cmdSaveChar :: Game -> IO (Game,String)
cmdSaveChar g@(Gm system char) = do saveCharacther char
                                    return (g, "")

cmdListAct :: Game -> IO (Game,String)
cmdListAct g@(Gm system char) = do putAndWait ((action_list (actionsFromSystem system) 1)++"Press any key to continue \n")
                                   return (g, "")

cmdMakeAct :: Game -> IO (Game,String)
cmdMakeAct g@(Gm system char) = do make_action system char
                                   return (g, "")

cmdModChar :: Game -> IO (Game,String)
cmdModChar g@(Gm system char) = do newChars <- modifyCharacter system char
                                   return ((Gm system newChars), "")

cmdDelChar :: Game -> IO (Game,String)
cmdDelChar g@(Gm system char) = do newChar <- delete_character char
                                   return ((Gm system newChar), "")

cmdLoadChar :: Game -> IO (Game,String)
cmdLoadChar g@(Gm system char) = do newChar <- loadCharaterInterface
                                    return ((Gm system (newChar++char)), "")

cmdShowChar :: Game -> IO (Game,String)
cmdShowChar g@(Gm system char) = do showCharacter char
                                    return (g, "")

cmdExit :: Game -> IO (Game,String)
cmdExit g = do System.Process.system "clear"
               putStr "Goodbye!...\n"
               return (g,"Exit")

menu_title :: [(String,(Game -> IO (Game,String)))]
menu_title = [("Create new Character",cmdNewChar)
             ,("Roll attributes/skills",cmdRollAtr)
             ,("List Character",cmdListChar)
             ,("Save Character",cmdSaveChar)
             ,("List Action",cmdListAct)
             ,("Make an action",cmdMakeAct)
             ,("Modify character",cmdModChar)
             ,("Delete Character",cmdDelChar)
             ,("Load Character",cmdLoadChar)
             ,("View Character",cmdShowChar)
             ,("Exit",cmdExit)]
