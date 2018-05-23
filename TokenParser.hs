 module TokenParser where

import Data.Char
import Control.Monad
import Control.Applicative

data LexToken =   KEYW_IF 
                | KEYW_ELSE 
                | KEYW_THEN 
                | KEYW_WHILE 
                | KEYW_DO 
                | KEYW_READ
                | KEYW_WRITE
                | KEYW_ELIF
                | Liter String
                | LNum Float 
                | Op Oper 
                | AOp AOper
                | Sep Separ deriving (Show, Eq)               
                
data Oper = Oplus | Ominus | Omul | Odiv | Omod | Oeq | One | Ogt | Oge | Olt | Ole | Oand | Oor | Oas deriving (Show, Eq)
data AOper = OAplus | OAminus | OAmul | OAdiv | OAmod deriving (Show, Eq)
data Separ = CBracket_L | CBracket_R | Bracket_L | Bracket_R | Comma | Semicolon deriving (Show, Eq)

data Ident = Ident String deriving (Show, Eq)
data PNum = PNum Float deriving (Show, Eq)
data Args = Args [Expr] deriving (Show, Eq)
data ArgDef = ArgDef [Ident] deriving (Show, Eq)
data FunDef = FunDef Ident ArgDef Body deriving (Show, Eq)
data Expr = ExprCall FunCall | ExprId Ident | ExprNum PNum | ExprOp Oper Expr Expr deriving (Show, Eq)
data FunCall = FunCall Ident Args deriving (Show, Eq)
data Body = Body [Command] deriving (Show, Eq)
data Command = ComCall FunCall | ComAs Ident Expr | ComWrite Expr | ComRead Ident | ComWhile Expr Body | ComIf Expr Body Body deriving (Show, Eq)
data Program = Program [FunDef] Body deriving (Show, Eq)

newtype TParser a = TParser { parse :: [LexToken] -> [(a,[LexToken])] }
             
t_runParser :: TParser a -> [LexToken] -> a
t_runParser m s =
  case parse m s of
    [(res, [])] -> res
    _           -> error "Parsing error!"

t_item :: TParser LexToken
t_item = TParser $ \s ->
  case s of
   []     -> []
   (c:cs) -> [(c,cs)]

instance Functor TParser where
  fmap f (TParser cs) = TParser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative TParser where
  pure = return
  (TParser cs1) <*> (TParser cs2) = TParser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

t_bind :: TParser a -> (a -> TParser b) -> TParser b
t_bind p f = TParser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

t_unit :: a -> TParser a
t_unit a = TParser (\s -> [(a,s)])

instance Monad TParser where
  return = t_unit
  (>>=)  = t_bind

instance MonadPlus TParser where
  mzero = t_failParse
  mplus = t_combine

instance Alternative TParser where
  empty = mzero
  (<|>) = t_option

t_combine :: TParser a -> TParser a -> TParser a
t_combine p q = TParser (\s -> parse p s ++ parse q s)

t_failParse :: TParser a
t_failParse = TParser (\cs -> [])

t_option :: TParser a -> TParser a -> TParser a
t_option  p q = TParser $ \s ->
  case parse p s of
    []  -> parse q s
    res -> res

t_satisfy :: (LexToken -> Bool) -> TParser LexToken
t_satisfy p = t_item `t_bind` \c ->
  if p c
  then t_unit c
  else t_failParse

t_oneOf :: [LexToken] -> TParser LexToken
t_oneOf s = t_satisfy (flip elem s)

t_chainl1 :: TParser a -> TParser (a -> a -> a) -> TParser a
p `t_chainl1` op = do 
    a <- p
    rest a
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a

checkLiter :: LexToken -> Bool
checkLiter (Liter _) = True
checkLiter _ = False
                 
getLiter :: TParser LexToken
getLiter = t_satisfy (checkLiter)

checkLNum :: LexToken -> Bool
checkLNum (LNum _) = True
checkLNum _ = False

getLNum :: TParser LexToken
getLNum = t_satisfy (checkLNum)
                 
checkOp :: LexToken -> Bool    
checkOp (Op _) = True
checkOp _ = False
                 
getOp :: TParser LexToken      
getOp = t_satisfy (checkOp) 
    
checkAOp :: LexToken -> Bool    
checkAOp (AOp _) = True
checkAOp _ = False
                 
getAOp :: TParser LexToken      
getAOp = t_satisfy (checkAOp)
                 
lexToken :: LexToken -> TParser LexToken
lexToken c = t_satisfy (c ==)

tokenSeq :: [LexToken] -> TParser [LexToken]
tokenSeq [] = return []
tokenSeq (c:cs) = do { lexToken c; tokenSeq cs; return (c:cs)}

t_reserved :: [LexToken] -> TParser [LexToken]
t_reserved [] = return []
t_reserved (c:cs) = do 
    lexToken c
    t_reserved cs
    return (c:cs)
    
t_expr = [Sep Bracket_L, KEYW_IF, Sep Bracket_R]

t_brackets :: TParser a -> TParser a
t_brackets m = do
    t_reserved ([Sep Bracket_L])
    n <- m
    t_reserved ([Sep Bracket_R])
    return n

parseIdent :: TParser Ident
parseIdent = do
    Liter s <- getLiter
    return $ Ident s
    
parsePNum :: TParser PNum
parsePNum = do 
    LNum n <- getLNum
    return $ PNum n
    
parseArgDef :: TParser ArgDef
parseArgDef = do { 
    x <- parseIdent;
    lexToken $ Sep Comma;
    ArgDef l <- parseArgDef;
    return $ ArgDef (x:l) 
} <|> do {
    x <- parseIdent;
    return $ ArgDef [x]
}

parseEmptyArgDef :: TParser ArgDef
parseEmptyArgDef = do {
    return $ ArgDef[]
}

parseFunDef :: TParser FunDef
parseFunDef = do {
    name <- parseIdent;
    t_reserved([Sep Bracket_L]);
    args <- (parseArgDef <|> parseEmptyArgDef);
    t_reserved([Sep Bracket_R]);
    t_reserved([Sep CBracket_L]);
    body <- parseBody;
    t_reserved([Sep CBracket_R]);
    return $ FunDef name args body
} <|> do {
    name <- parseIdent;
    t_reserved([Sep Bracket_L]);
    args <- (parseArgDef <|> parseEmptyArgDef);
    t_reserved([Sep Bracket_R]);
    t_reserved([Sep Semicolon]);
    return $ FunDef name args (Body [])
}

parseEmptyArgs :: TParser Args
parseEmptyArgs = do {
    return $ Args[]
}

parseArgs :: TParser Args
parseArgs = do {
    e <- parseExpr;
    lexToken $ Sep Comma;
    Args l <- parseArgs;
    return $ Args (e:l)
} <|> do {
    x <- parseExpr;
    return $ Args [x]
}

parseOp :: TParser Oper
parseOp = do
    Op op <- getOp
    return op

parseFunCall :: TParser FunCall
parseFunCall = do {
    name <- parseIdent;
    t_reserved([Sep Bracket_L]);
    args <- (parseArgs <|> parseEmptyArgs);
    t_reserved([Sep Bracket_R]);
    return $ FunCall name args
}

parseExpr :: TParser Expr
parseExpr = do {
    t_reserved([Sep Bracket_L]);    
    e <- parseExprNoBrackets;
    t_reserved([Sep Bracket_R]);  
    return e
} <|> parseExprNoBrackets

parseExprNoBrackets :: TParser Expr
parseExprNoBrackets = do {
    t_reserved([Sep Bracket_L]);  
    e1 <- parseExpr; 
    op <- parseOp;
    e2 <- parseExpr;
    t_reserved([Sep Bracket_R]);
    return $ ExprOp op e1 e2
} <|> do {
    call <- parseFunCall;
    return $ ExprCall call
} <|> do {
    id <- parseIdent;
    return $ ExprId id;
} <|> do {
    num <- parsePNum;
    return $ ExprNum num
}

parseBody :: TParser Body
parseBody = do {
    com <- parseCommand;
    Body l <- parseBody;
    return $ Body (com:l)
} <|> do {
    com <- parseCommand;
    return $ Body [com]
}

aopToOp :: AOper -> Oper
aopToOp OAplus = Oplus
aopToOp OAminus = Ominus
aopToOp OAdiv = Odiv
aopToOp OAmul = Omul
aopToOp OAmod = Omod

parseCommand :: TParser Command
parseCommand = do {
    id <- parseIdent;
    t_reserved([Op Oas]);    
    expr <- parseExpr;
    t_reserved([Sep Semicolon]);    
    return $ ComAs id expr
} <|> do {
    id <- parseIdent;
    AOp aop <- getAOp;   
    expr <- parseExpr;
    t_reserved([Sep Semicolon]);    
    return $ ComAs id (ExprOp (aopToOp aop) (ExprId id) expr)
} <|> do {
    call <- parseFunCall;
    t_reserved([Sep Semicolon]);  
    return $ ComCall call
} <|> do {
    t_reserved([KEYW_READ]);  
    t_reserved([Sep Bracket_L]);
    id <- parseIdent;
    t_reserved([Sep Bracket_R]);
    t_reserved([Sep Semicolon]);  
    return $ ComRead id;
} <|> do {
    t_reserved([KEYW_WRITE]);  
    t_reserved([Sep Bracket_L]);
    expr <- parseExpr;
    t_reserved([Sep Bracket_R]);
    t_reserved([Sep Semicolon]);  
    return $ ComWrite expr;
} <|> do {
    t_reserved([KEYW_WHILE]);  
    t_reserved([Sep Bracket_L]);
    expr <- parseExpr;
    t_reserved([Sep Bracket_R]);
    t_reserved([KEYW_DO]); 
    t_reserved([Sep CBracket_L]);
    body <- parseBody;
    t_reserved([Sep CBracket_R]);
    return $ ComWhile expr body;
} <|> do {
    t_reserved([KEYW_IF]);  
    t_reserved([Sep Bracket_L]);
    expr <- parseExpr;
    t_reserved([Sep Bracket_R]);
    t_reserved([KEYW_THEN]);
    t_reserved([Sep CBracket_L]);
    body1 <- parseBody;
    t_reserved([Sep CBracket_R]);
    t_reserved([KEYW_ELSE]);
    t_reserved([Sep CBracket_L]);
    body2 <- parseBody;
    t_reserved([Sep CBracket_R]);
    return $ ComIf expr body1 body2
} <|> do {
    t_reserved([KEYW_IF]);  
    t_reserved([Sep Bracket_L]);
    expr <- parseExpr;
    t_reserved([Sep Bracket_R]);
    t_reserved([KEYW_THEN]);
    t_reserved([Sep CBracket_L]);
    body1 <- parseBody;
    t_reserved([Sep CBracket_R]);
    return $ ComIf expr body1 (Body []) 
}

parseFunDefs :: TParser [FunDef]
parseFunDefs = do {
    def <- parseFunDef;
    l <- parseFunDefs;
    return (def:l)
} <|> do {
    def <- parseFunDef;
    return [def]
}

parseProgram :: TParser Program
parseProgram = do {
    defs <- parseFunDefs;
    body <- parseBody;
    return $ Program defs body
} <|> do {
    body <- parseBody;
    return $ Program [] body
}