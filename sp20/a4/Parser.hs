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
  , Tok.reservedOpNames = ["+", "=", "==", ">=", ">", "<=", "<", "||", "&&", "not"]
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

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

-- prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
-- prefixOp s f = Ex.Prefix (reservedOp s >> return f)

intLit :: Parser R2Expr
intLit = Tok.integer lexer >>= return . IntE . fromIntegral

identifier :: Parser String
identifier = Tok.identifier lexer

var :: Parser R2Expr
var = identifier >>= return . VarE

-- funCall :: Parser R2Expr
-- funCall = do
--   f <- identifier
--   parens whiteSpace
--   return $ FunCallE f
  

-- Operator table
table :: Ex.OperatorTable String () Identity R2Expr
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

expr :: Parser R2Expr
expr = Ex.buildExpressionParser table factor

letExpr :: Parser R2Expr
letExpr = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return $ LetE x e1 e2

ifExpr :: Parser R2Expr
ifExpr = do
  reserved "if"
  e1 <- expr
  reserved "then"
  e2 <- expr
  reserved "else"
  e3 <- expr
  return $ IfE e1 e2 e3

trueExpr :: Parser R2Expr
trueExpr = do
  reserved "True"
  return TrueE

falseExpr :: Parser R2Expr
falseExpr = do
  reserved "False"
  return FalseE

factor :: Parser R2Expr
factor =
      intLit
  -- <|> try funCall
  <|> trueExpr
  <|> falseExpr
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

parseExpr :: String -> Either ParseError R2Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseFile :: String -> IO R2Expr
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
