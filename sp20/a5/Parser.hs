module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Data.Functor.Identity

import AST

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames   = ["let", "in", "if", "then", "else", "True", "False"]
  , Tok.reservedOpNames = ["+", "=", "==", ">=", ">", "<=", "<",
                           "||", "&&", "not",
                           "vector", "vectorRef", "vectorSet", "void"]
  , Tok.caseSensitive   = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

comma :: Parser String
comma = Tok.comma lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Tok.commaSep1 lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

-- prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
-- prefixOp s f = Ex.Prefix (reservedOp s >> return f)

intLit :: Parser Int
intLit = Tok.integer lexer >>= return . fromIntegral

intExpr :: Parser R3Expr
intExpr = intLit >>= return . IntE

identifier :: Parser String
identifier = Tok.identifier lexer

var :: Parser R3Expr
var = identifier >>= return . VarE

-- funCall :: Parser R3Expr
-- funCall = do
--   f <- identifier
--   parens whiteSpace
--   return $ FunCallE f

-- Operator table
table :: Ex.OperatorTable String () Identity R3Expr
table   = [ [ prefixOp "not" NotE ]
          , [ infixOp "+" (PlusE) Ex.AssocLeft ]
          , [ infixOp "<" (CmpE CmpLT) Ex.AssocLeft
            , infixOp "<=" (CmpE CmpLTE) Ex.AssocLeft
            , infixOp ">" (CmpE CmpGT) Ex.AssocLeft
            , infixOp ">=" (CmpE CmpGTE) Ex.AssocLeft
            ]
          , [ infixOp "==" (CmpE CmpEqual) Ex.AssocLeft ]
          , [ infixOp "&&" (AndE) Ex.AssocLeft
            , infixOp "||" (OrE) Ex.AssocLeft ]
          ]

prefixOp name fun = Ex.Prefix (do{ reservedOp name; return fun })
infixOp name fun assoc = Ex.Infix (do{ reservedOp name; return fun }) assoc

expr :: Parser R3Expr
expr = Ex.buildExpressionParser table factor

letExpr :: Parser R3Expr
letExpr = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return $ LetE x e1 e2

ifExpr :: Parser R3Expr
ifExpr = do
  reserved "if"
  e1 <- expr
  reserved "then"
  e2 <- expr
  reserved "else"
  e3 <- expr
  return $ IfE e1 e2 e3

trueExpr :: Parser R3Expr
trueExpr = do
  reserved "True"
  return TrueE

falseExpr :: Parser R3Expr
falseExpr = do
  reserved "False"
  return FalseE

voidExpr :: Parser R3Expr
voidExpr = reservedOp "void" >> return VoidE

vectorSetExpr :: Parser R3Expr
vectorSetExpr = do
  reservedOp "vectorSet"
  (e1, idx, e2) <- parens (do
                                 e1 <- expr
                                 comma
                                 idx <- intLit
                                 comma
                                 e2 <- expr
                                 return (e1, idx, e2)
                             )
  return $ VectorSetE e1 idx e2

vectorRefExpr :: Parser R3Expr
vectorRefExpr = do
  reservedOp "vectorRef"
  (e1, idx) <- parens (do
                             e1 <- expr
                             comma
                             idx <- intLit
                             return (e1, idx)
                         )
  return $ VectorRefE e1 idx

vectorExpr :: Parser R3Expr
vectorExpr = do
  reservedOp "vector"
  args <- parens $ commaSep expr
  return $ VectorE args


factor :: Parser R3Expr
factor =
      intExpr
  -- <|> try funCall
  <|> voidExpr
  <|> vectorSetExpr
  <|> vectorRefExpr
  <|> vectorExpr
  <|> trueExpr
  <|> falseExpr
  <|> voidExpr
  <|> try ifExpr
  <|> var
  <|> letExpr
  <|> parens expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr :: String -> Either ParseError R3Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseFile :: String -> IO R3Expr
parseFile fileName = do
  s <- readFile fileName
  case parseExpr s of
    Right e -> return e
    Left err -> error $ "Failed to parse " ++ fileName ++ ": " ++ (show err)

main :: IO ()
main = do
  putStrLn $ show $ parseExpr "0 + (1 + x)"
  putStrLn $ show $ parseExpr "let x = 5 + z in x + y"
  e <- parseFile "testfile.r1"
  putStrLn $ show e
