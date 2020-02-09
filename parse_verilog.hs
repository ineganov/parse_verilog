import System.Environment
import Data.Char
import Control.Monad.State.Lazy (State, get, put, runState)

data Token = EOF
           | Comment String
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
           | BaseHex FS_value | BaseDec FS_value | BaseOct FS_value | BaseBin FS_value
           | Comma deriving (Show, Eq)

data ModuleDecl       = ModuleDecl String [String] [ModuleItem] deriving (Show)
data ModuleConn       = ModuleConn String Expr   deriving (Show)
data ModulePara       = ModulePara String Expr   deriving (Show)
data Range            = Range Expr Expr deriving (Show)

data Expr             = E_Variable String
                      | E_Number   Int
                      | E_Based    Int FS_value
                      | E_IndexOp  String Expr deriving (Show)

data ModuleItem       = MItem_Output (Maybe Range) [String]
                      | MItem_Input  (Maybe Range) [String] 
                      | MItem_Wire   (Maybe Range) [String]
                      | MItem_Reg    (Maybe Range) [String]
                      | MItem_Inst   String String [ModulePara] [ModuleConn] deriving (Show)


data FourState        = S_0 | S_1 | S_x | S_z deriving (Eq, Ord, Enum)
type FS_value         = [FourState]

instance Show FourState where
   show S_0 = "0"
   show S_1 = "1"
   show S_x = "x"
   show S_z = "z"

fs_sum :: FS_value -> FS_value -> FS_value
fs_sum a b = undefined

fs_sum_full_adder :: FourState -> FourState -> FourState -> (FourState, FourState)
--                             Carry  Rslt
fs_sum_full_adder S_0 S_0 S_0 = (S_0, S_0)
fs_sum_full_adder S_0 S_0 S_1 = (S_0, S_1)
fs_sum_full_adder S_0 S_1 S_0 = (S_0, S_1)
fs_sum_full_adder S_1 S_0 S_0 = (S_0, S_1)
fs_sum_full_adder S_1 S_1 S_0 = (S_1, S_0)
fs_sum_full_adder S_1 S_0 S_1 = (S_1, S_0)
fs_sum_full_adder S_0 S_1 S_1 = (S_1, S_0)
fs_sum_full_adder S_1 S_1 S_1 = (S_1, S_1)
fs_sum_full_adder _   _   _   = (S_x, S_x)


fs_from_int :: Int -> Int -> FS_value
fs_from_int n x = reverse $ iter n x
                     where iter 0 _ = []
                           iter n x = (mod_val $ mod x 2) : (iter (n-1) $ div x 2)
                           mod_val 0 = S_0
                           mod_val _ = S_1

parse_bin :: String -> FS_value
parse_bin x = parse_bin' $ map toLower x
              where parse_bin' [] = []
                    parse_bin' ('_':rest) =       parse_bin' rest
                    parse_bin' ('0':rest) = S_0 : parse_bin' rest
                    parse_bin' ('1':rest) = S_1 : parse_bin' rest
                    parse_bin' ('x':rest) = S_x : parse_bin' rest
                    parse_bin' ('z':rest) = S_z : parse_bin' rest

parse_oct :: String -> FS_value
parse_oct x = parse_oct' $ map toLower x
              where parse_oct' [] = []
                    parse_oct' ('_':rest) = parse_oct' rest
                    parse_oct' ('x':rest) = [S_x, S_x, S_x] ++ (parse_oct' rest)
                    parse_oct' ('z':rest) = [S_z, S_z, S_z] ++ (parse_oct' rest)
                    parse_oct' (x:rest)   = (fs_from_int 3 $ fromEnum x - fromEnum '0') ++ (parse_oct' rest)

parse_dec :: String -> FS_value
parse_dec = undefined

parse_hex :: String -> FS_value
parse_hex x = parse_hex' $ map toLower x
              where parse_hex' [] = []
                    parse_hex' ('_':rest) = parse_hex' rest
                    parse_hex' ('x':rest) = [S_x, S_x, S_x, S_x] ++ (parse_hex' rest)
                    parse_hex' ('z':rest) = [S_z, S_z, S_z, S_z] ++ (parse_hex' rest)
                    parse_hex' (x:rest)   = if x <= '9' 
                                               then (fs_from_int 4 $ fromEnum x - fromEnum '0') ++ (parse_hex' rest)
                                               else (fs_from_int 4 $ 10 + fromEnum x - fromEnum 'a') ++ (parse_hex' rest)


pp :: Show a => [a] -> IO ()
pp = mapM_ (putStrLn . show)

tokenize :: String -> [Token]
tokenize []           = [EOF]
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
tokenize ('\'':'h':xs) = BaseHex  (parse_hex digits) : tokenize rest where (digits, rest) = span (\c -> c `elem` "xXzZ_0123456789abcdeABCDE") xs
tokenize ('\'':'d':xs) = BaseDec  (parse_dec digits) : tokenize rest where (digits, rest) = span (\c -> c `elem` "xXzZ_0123456789") xs
tokenize ('\'':'o':xs) = BaseOct  (parse_oct digits) : tokenize rest where (digits, rest) = span (\c -> c `elem` "xXzZ_01234567") xs
tokenize ('\'':'b':xs) = BaseBin  (parse_bin digits) : tokenize rest where (digits, rest) = span (\c -> c `elem` "xXzZ_01") xs
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

parse_file :: FilePath -> IO ModuleDecl
parse_file fname = tokenize_file fname >>= (\tkns -> return $ fst $ runState parse_mdl_decl tkns)

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

numbr :: State [Token] Int
numbr = get >>= (\c -> case c of
                       ((NumLiteral x):xs) -> put xs >> return x
                       otherwise -> error "Expected a numeric literal" )

expt :: Token -> State [Token] ()
expt t = get >>= (\(x:xs) -> if x == t then put xs 
                                       else error $ "Expected token <" ++ show t ++ ">, but got: " ++ show x)

-----------------------------------------------------------------------------------------------------------------------

parse_mdl_decl :: State [Token] ModuleDecl
parse_mdl_decl = do expt K_module
                    modname  <- idnt
                    expt LParen
                    modports <- parse_ident_list RParen
                    expt RParen
                    expt Semi
                    moditems <- parse_moditems
                    expt K_endmodule
                    expt EOF -- FIXME: allow for multiple modules
                    return $ ModuleDecl modname modports moditems

parse_ident_list :: Token -> State [Token] [String]
parse_ident_list fin = do  first <- idnt -- won't accept empty list
                           c     <- cur
                           if c == Comma then do adv
                                                 rest <- parse_ident_list fin
                                                 return $ first : rest
                           else if c == fin then return $ first : []
                           else error $ "Expected Comma or " ++ (show fin) ++ " at the end of port list"


parse_moditems :: State [Token] [ModuleItem]
parse_moditems = do  c <- cur
                     if c == K_endmodule
                        then return []
                        else do item_head  <- parse_moditem
                                items_rest <- parse_moditems
                                return $ item_head : items_rest

parse_var_decl :: (Maybe Range -> [String] -> ModuleItem) -> State [Token] ModuleItem
parse_var_decl cons = do adv
                         rng <- parse_range
                         nms <- parse_ident_list Semi
                         expt Semi
                         return $ cons rng nms

parse_moditem :: State [Token] ModuleItem
parse_moditem = do  c <- cur
                    case c of
                       K_input   -> parse_var_decl MItem_Input
                       K_output  -> parse_var_decl MItem_Output
                       K_wire    -> parse_var_decl MItem_Wire
                       K_reg     -> parse_var_decl MItem_Reg
                       (Ident _) -> parse_mod_inst
                       _ -> error "Unexpected module item"

parse_conn :: State [Token] ModuleConn -- FIXME: allow explicit empty connection
parse_conn = do expt Dot
                conn_a <- idnt
                expt LParen
                conn_expr <- parse_expr
                expt RParen
                return $ ModuleConn conn_a conn_expr

parse_para :: State [Token] ModulePara
parse_para = do expt Dot
                para_a <- idnt
                expt LParen
                para_expr <- parse_expr
                expt RParen
                return $ ModulePara para_a para_expr

parse_conns :: State [Token] [ModuleConn]
parse_conns = do conn <- parse_conn
                 c <- cur
                 case c of
                     Comma -> do adv
                                 conns <- parse_conns
                                 return $ conn : conns
                     _     ->    return $ conn : []

parse_params :: State [Token] [ModulePara] -- accepts #( .at_least_one (param) ) or nothing
parse_params = do c <- cur
                  case c of
                     Hash -> do adv
                                expt LParen
                                para <- parse_params_
                                expt RParen
                                return para
                     _    -> return []

parse_params_ :: State [Token] [ModulePara]
parse_params_ = do para <- parse_para
                   c <- cur
                   case c of
                     Comma -> do adv
                                 params <- parse_params_
                                 return $ para : params
                     _     ->    return $ para : []

parse_mod_inst :: State [Token] ModuleItem
parse_mod_inst = do nm_m <- idnt
                    para <- parse_params
                    nm_i <- idnt
                    expt LParen
                    conns <- parse_conns
                    expt RParen
                    expt Semi
                    return $ MItem_Inst nm_m nm_i para conns

parse_range :: State [Token] (Maybe Range)
parse_range = do c <- cur
                 case c of
                     LBracket -> do adv
                                    hi <- parse_expr
                                    expt Colon
                                    lo <- parse_expr
                                    expt RBracket
                                    return $ Just $ Range hi lo
                     _ -> return Nothing

parse_expr :: State [Token] Expr
parse_expr = do c <- cur
                case c of
                  (Ident s)      -> do adv
                                       cc <- cur
                                       if cc == LBracket then do adv
                                                                 idx_expr <- parse_expr
                                                                 expt RBracket
                                                                 return $ E_IndexOp s idx_expr
                                                         else    return $ E_Variable s
                  (NumLiteral i) -> do adv
                                       cc <- cur
                                       case cc of
                                          (BaseHex s) -> adv >> (return $ E_Based i s)
                                          (BaseDec s) -> adv >> (return $ E_Based i s)
                                          (BaseOct s) -> adv >> (return $ E_Based i s)
                                          (BaseBin s) -> adv >> (return $ E_Based i s)
                                          _           -> return $ E_Number i
                  (BaseHex s)    -> adv >> (return $ E_Based 32 s)
                  (BaseDec s)    -> adv >> (return $ E_Based 32 s)
                  (BaseOct s)    -> adv >> (return $ E_Based 32 s)
                  (BaseBin s)    -> adv >> (return $ E_Based 32 s)
                  _              -> error "Failed to parse expression :("

main = undefined
