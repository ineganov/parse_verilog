import System.Environment
import Data.Char
import Control.Monad.State.Lazy (State, get, put, runState)

data Token = Comment String 
           | Ident String
           | NumLiteral Int 
           | OpAssign | OpBitwiseOr | OpBitwiseAnd | OpBitwiseXor | OpBitwiseInv
           | K_module | K_endmodule | K_begin | K_end | K_assign | K_wire | K_reg | K_input | K_output | K_inout
           | LParen   | RParen 
           | LBracket | RBracket 
           | LBrace   | RBrace
           | Semi 
           | Colon
           | Hash
           | Dot 
           | BaseHex | BaseDec | BaseOct | BaseBin
           | Comma deriving (Show, Eq)


data ModuleDecl       = ModuleDecl String ModulePorts [ModuleItem]        deriving (Show)
data ModulePorts      = ModulePorts String ModuleOtherPorts               deriving (Show)
data ModuleOtherPorts = ModuleOtherPort ModulePorts | ModuleNoOtherPorts  deriving (Show)

data ModuleItem       = MItem_Output String
                      | MItem_Input  String deriving (Show)


pp :: Show a => [a] -> IO ()
pp = mapM_ (putStrLn . show)

tokenize :: String -> [Token]
tokenize []           = []
tokenize (' ':xs)     = tokenize xs
tokenize ('\n':xs)    = tokenize xs
tokenize ('(':xs)     = LParen   : tokenize xs
tokenize (')':xs)     = RParen   : tokenize xs
tokenize ('[':xs)     = LBracket : tokenize xs
tokenize (']':xs)     = RBracket : tokenize xs
tokenize ('{':xs)     = LBrace   : tokenize xs
tokenize ('}':xs)     = RBrace   : tokenize xs
tokenize (';':xs)     = Semi     : tokenize xs
tokenize (':':xs)     = Colon    : tokenize xs
tokenize (',':xs)     = Comma    : tokenize xs
tokenize ('#':xs)     = Hash     : tokenize xs
tokenize ('.':xs)     = Dot      : tokenize xs
tokenize ('=':xs)     = OpAssign     : tokenize xs
tokenize ('&':xs)     = OpBitwiseAnd : tokenize xs
tokenize ('|':xs)     = OpBitwiseOr  : tokenize xs
tokenize ('^':xs)     = OpBitwiseXor : tokenize xs
tokenize ('~':xs)     = OpBitwiseInv : tokenize xs
tokenize ('\\':xs)    = Ident   idnt : tokenize rest where (idnt, rest) = break (== ' ' ) xs 
tokenize ('/':'/':xs) = Comment cmnt : tokenize rest where (cmnt, rest) = break (== '\n') xs 
tokenize ('\'':'h':xs) = BaseHex  : tokenize xs
tokenize ('\'':'d':xs) = BaseDec  : tokenize xs
tokenize ('\'':'o':xs) = BaseOct  : tokenize xs
tokenize ('\'':'b':xs) = BaseBin  : tokenize xs
tokenize (x:xs) | isDigit x = NumLiteral (read digits) : tokenize rest where (digits, rest) = span isDigit (x:xs) 
tokenize (x:xs) | isAlpha x = case span isLegalIdent (x:xs) of
                                 ("module"    , rest)  -> K_module    : tokenize rest
                                 ("endmodule" , rest)  -> K_endmodule : tokenize rest
                                 ("begin"     , rest)  -> K_begin     : tokenize rest
                                 ("end"       , rest)  -> K_end       : tokenize rest
                                 ("assign"    , rest)  -> K_assign    : tokenize rest
                                 ("wire"      , rest)  -> K_wire      : tokenize rest
                                 ("reg"       , rest)  -> K_reg       : tokenize rest
                                 ("input"     , rest)  -> K_input     : tokenize rest
                                 ("output"    , rest)  -> K_output    : tokenize rest
                                 ("inout"     , rest)  -> K_inout     : tokenize rest
                                 ( other      , rest)  -> Ident other : tokenize rest
tokenize err = error $ "Unexpected token: " ++ take 10 err

isLegalIdent :: Char -> Bool
isLegalIdent c = isAlphaNum c || c == '_' || c == '$'

no_comments :: [Token] -> [Token]
no_comments []             = []
no_comments (Comment _:xs) = no_comments xs
no_comments (x:xs)         = x:(no_comments xs)

tokenize_file :: String -> IO [Token]
tokenize_file s = readFile s >>= (return . no_comments . tokenize)



-----------------------------------------------------------------------------------------------------------------------

cur :: State [Token] Token
cur = get >>= (\c -> case c of
                       (x:_)     -> return x
                       otherwise -> error "Unexpected end of input (cur)" )

adv :: State [Token] ()
adv = get >>= (\c -> case c of
                       (_:xs)    -> put xs
                       otherwise -> error "Unexpected end of input (adv)" )

idnt :: State [Token] String
idnt = get >>= (\c -> case c of
                       ((Ident x):xs) -> put xs >> return x
                       otherwise -> error "Expected an identifier" )

expt :: Token -> State [Token] ()
expt t = get >>= (\(x:xs) -> if x == t then put xs 
                                       else error $ "Expected token <" ++ show t ++ ">, but got: " ++ show x)

-----------------------------------------------------------------------------------------------------------------------

parse_mdl_decl :: State [Token] ModuleDecl
parse_mdl_decl = do expt K_module
                    modname  <- idnt
                    expt LParen
                    modports <- parse_modports
                    expt RParen
                    expt Semi
                    moditems <- parse_moditems
                    expt K_endmodule
                    return $ ModuleDecl modname modports moditems

parse_modports :: State [Token] ModulePorts
parse_modports = do port <- idnt
                    otherports <- parse_other_ports
                    return $ ModulePorts port otherports

parse_other_ports :: State [Token] ModuleOtherPorts
parse_other_ports =  do c <- cur
                        case c of
                           Comma -> do adv
                                       more <- parse_modports
                                       return $ ModuleOtherPort more
                           RParen ->   return   ModuleNoOtherPorts

parse_moditems :: State [Token] [ModuleItem]
parse_moditems = do  c <- cur
                     if c == K_endmodule
                        then return []
                        else do item_head  <- parse_moditem
                                items_rest <- parse_moditems
                                return $ item_head : items_rest



parse_moditem :: State [Token] ModuleItem
parse_moditem = do  c <- cur
                    case c of
                       K_input  -> do adv
                                      nm <- idnt
                                      expt Semi
                                      return $ MItem_Input nm
                        
                       K_output -> do adv
                                      nm <- idnt
                                      expt Semi
                                      return $ MItem_Output nm

                       otherwise -> error "Unexpected module item"

main = undefined
