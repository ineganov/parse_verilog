import System.Environment
import Data.Char
import Data.Map (fromList, member, (!))
import Control.Monad.State.Lazy (State, get, put, runState)

data Token = Comment String 
           | Ident String
           | DotConn String
           | NumLiteral Int 
           | OpAssign | OpBitwiseOr | OpBitwiseAnd | OpBitwiseXor | OpBitwiseInv
           | K_module | K_endmodule | K_begin | K_end | K_assign | K_wire | K_reg | K_input | K_output | K_inout
           | LParen   | RParen 
           | LBracket | RBracket 
           | LBrace   | RBrace
           | Semi 
           | Colon
           | Hash 
           | BaseHex | BaseDec | BaseOct | BaseBin
           | Comma deriving (Show, Eq)


data Dir   = Input | Output | Inout  deriving (Show)
data Width = Bus Int Int | Single    deriving (Show)
data Port  = Port String Dir Width   deriving (Show)

data Module = Module String [Port]   deriving (Show)

keyw_map = fromList [ ("module",    K_module    ),
                      ("endmodule", K_endmodule ),
                      ("begin",     K_begin     ),
                      ("end",       K_end       ),
                      ("assign",    K_assign    ),
                      ("wire",      K_wire      ),
                      ("reg",       K_reg       ),
                      ("input",     K_input     ),
                      ("output",    K_output    ),
                      ("inout",     K_inout     ) ]

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
tokenize ('=':xs)     = OpAssign     : tokenize xs
tokenize ('&':xs)     = OpBitwiseAnd : tokenize xs
tokenize ('|':xs)     = OpBitwiseOr  : tokenize xs
tokenize ('^':xs)     = OpBitwiseXor : tokenize xs
tokenize ('~':xs)     = OpBitwiseInv : tokenize xs
tokenize ('\\':xs)    = Ident   (takeWhile (/= ' ') xs)     : tokenize (dropWhile (/= ' ') xs)
tokenize ('.':xs)     = DotConn (takeWhile isLegalIdent xs) : tokenize (dropWhile isLegalIdent xs)
tokenize ('/':'/':xs) = Comment (takeWhile (/= '\n') xs)    : tokenize (dropWhile (/= '\n') xs)
tokenize ('\'':'h':xs) = BaseHex  : tokenize xs
tokenize ('\'':'d':xs) = BaseDec  : tokenize xs
tokenize ('\'':'o':xs) = BaseOct  : tokenize xs
tokenize ('\'':'b':xs) = BaseBin  : tokenize xs
tokenize (x:xs) | isAlpha x = if member anum keyw_map 
                                then (keyw_map ! anum)                     : tokenize (dropWhile isLegalIdent xs)
                                else Ident anum                            : tokenize (dropWhile isLegalIdent xs)
                | isDigit x = NumLiteral (read (takeWhile isDigit (x:xs))) : tokenize (dropWhile isDigit xs)
                | otherwise = error $ "Unexpected token: " ++ take 10 (x:xs)
                where anum = takeWhile isLegalIdent (x:xs)

isLegalIdent :: Char -> Bool
isLegalIdent c = isAlphaNum c || c == '_' || c == '$'

no_comments :: [Token] -> [Token]
no_comments []             = []
no_comments (Comment _:xs) = no_comments xs
no_comments (x:xs)         = x:(no_comments xs)

tokenize_file :: String -> IO [Token]
tokenize_file s = readFile s >>= (return . no_comments . tokenize)


expect :: Token -> State [Token] ()
expect t = get >>= (\(x:xs) -> if x == t then put xs else error $ "Expected token <" ++ show t ++ ">, but got: " ++ show x)

ident_value :: State [Token] String
ident_value = do (i:xs) <- get
                 case i of
                  (Ident s) -> put xs >> return s
                  otherwise -> error $ "Expected an identifier, but got: " ++ show i

consume :: Int -> State [Token] ()
consume i = get >>= (put . (drop i))

parse_port :: State [Token] Port
parse_port = do st <- get
                case st of
                  (K_input :Ident s:xs) -> consume 2 >> return (Port s Input  Single)
                  (K_inout :Ident s:xs) -> consume 2 >> return (Port s Inout  Single)
                  (K_output:Ident s:xs) -> consume 2 >> return (Port s Output Single)
                  (K_input :LBracket:NumLiteral hi:Colon:NumLiteral lo:RBracket:Ident s:xs) -> consume 7 >> return (Port s Input  (Bus hi lo))
                  (K_inout :LBracket:NumLiteral hi:Colon:NumLiteral lo:RBracket:Ident s:xs) -> consume 7 >> return (Port s Inout  (Bus hi lo))
                  (K_output:LBracket:NumLiteral hi:Colon:NumLiteral lo:RBracket:Ident s:xs) -> consume 7 >> return (Port s Output (Bus hi lo))
                  otherwise             -> error $ "Expected a port spec but got: " ++ (show $ take 7 st)

parse_ports :: State [Token] [Port]
parse_ports = do expect LParen
                 ports <- parse_ports_loop []
                 expect RParen
                 expect Semi
                 return ports


parse_ports_loop :: [Port] -> State [Token] [Port]
parse_ports_loop pp = do p <- parse_port
                         s <- get
                         let current_plist = pp ++ [p]
                         if head s /= Comma then return current_plist
                                            else consume 1 >> parse_ports_loop current_plist

parse_modul :: State [Token] Module
parse_modul =  do s <- get
                  expect K_module
                  nm <- ident_value
                  pp <- parse_ports
                  return $ Module nm pp

main = (fmap head getArgs) >>= tokenize_file >>= return . (runState parse_modul) >>= putStrLn . show


