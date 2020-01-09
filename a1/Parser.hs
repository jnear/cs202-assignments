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
  , Tok.reservedNames   = ["let", "in"]
  , Tok.reservedOpNames = ["+", "="]
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

prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)

intLit :: Parser R0Expr
intLit = Tok.integer lexer >>= return . IntE . fromIntegral

identifier :: Parser String
identifier = Tok.identifier lexer

-- Operator table
table :: Ex.OperatorTable String () Identity R0Expr
table   = [ [infixOp "+" (PlusE) Ex.AssocLeft ]
           ]

infixOp name fun assoc = Ex.Infix (do{ reservedOp name; return fun }) assoc

expr :: Parser R0Expr
expr = Ex.buildExpressionParser table factor

factor :: Parser R0Expr
factor =
      intLit
  <|> parens expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr :: String -> Either ParseError R0Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseFile :: String -> IO R0Expr
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
