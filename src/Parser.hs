module Parser (parseString) where

import Control.Monad.Except
import Data.Functor.Identity
import Data.Bifunctor

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

import AST

{-- Language Definition --}
keywords :: [String]
keywords =
    ["class",
     "method",
     "call",
     "uncall",
     "int",
     "new",
     "delete"]

--Operator precedence identical to C
operatorTable :: [[(String, BinOp)]]
operatorTable =[ [("+", Add)] ]

languageDef :: Token.LanguageDef st
languageDef =
    emptyDef {
        Token.commentLine     = "//",
        Token.nestedComments  = False,
        Token.identStart      = letter,
        Token.identLetter     = alphaNum <|> oneOf "_'",
        Token.reservedOpNames = concatMap (map fst) operatorTable,
        Token.reservedNames   = keywords,
        Token.caseSensitive   = True }

tokenParser :: Token.TokenParser st
tokenParser = Token.makeTokenParser languageDef

{-- Parser Primitives --}
identifier :: Parser String
identifier = Token.identifier tokenParser

reserved :: String -> Parser ()
reserved = Token.reserved tokenParser

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp tokenParser

integer :: Parser Integer
integer = Token.integer tokenParser

symbol :: String -> Parser String
symbol = Token.symbol tokenParser

parens :: Parser a -> Parser a
parens = Token.parens tokenParser

colon :: Parser String
colon = Token.colon tokenParser

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep tokenParser

typeName :: Parser TypeName
typeName = identifier

methodName :: Parser MethodName
methodName = identifier

{-- Expression Parsers --}
constant :: Parser Expression
constant = Constant <$> integer

variable :: Parser Expression
variable = Variable <$> identifier

nil :: Parser Expression
nil = Nil <$ reserved "nil"

expression :: Parser Expression
expression = buildExpressionParser opTable $ constant <|> variable <|> nil
    where binop (t, op) = Infix (Binary op <$ reservedOp t) AssocLeft
          opTable = (map . map) binop operatorTable

{-- Statement Parsers --}
modOp :: Parser ModOp
modOp = ModAdd <$ symbol "+="

assign :: Parser Statement
assign = Assign <$> identifier <*> modOp <*> expression

objectCall :: Parser Statement
objectCall =
    reserved "call"
    >> ObjectCall
    <$> identifier
    <* colon
    <* colon
    <*> methodName
    <*> parens (commaSep identifier)

objectUncall :: Parser Statement
objectUncall =
    reserved "uncall"
    >> ObjectUncall
    <$> identifier
    <* colon
    <* colon
    <*> methodName
    <*> parens (commaSep identifier)

objectConstruction :: Parser Statement
objectConstruction =
    reserved "new"
    >> ObjectConstruction
    <$> typeName
    <*> identifier

objectDestruction :: Parser Statement
objectDestruction =
    reserved "delete"
    >> ObjectDestruction
    <$> typeName
    <*> identifier

statement :: Parser Statement
statement = try assign
        <|> objectCall
        <|> objectUncall
        <|> objectConstruction
        <|> objectDestruction

block :: Parser [Statement]
block = many1 statement

{-- Top Level Parsers --}
dataType :: Parser DataType
dataType = IntegerType <$ reserved "int" <|> ObjectType <$> typeName

variableDeclaration :: Parser VariableDeclaration
variableDeclaration = GDecl <$> dataType <*> identifier

methodDeclaration :: Parser MethodDeclaration
methodDeclaration =
    reserved "method"
    >> GMDecl
    <$> methodName
    <*> parens (commaSep variableDeclaration)
    <*> block

classDeclaration :: Parser ClassDeclaration
classDeclaration =
    reserved "class"
    >> GCDecl
    <$> typeName
    <*> many variableDeclaration
    <*> many1 methodDeclaration

program :: Parser Program
program = spaces >> GProg <$> many1 classDeclaration <* eof

parseString :: String -> Except String Program
parseString s = ExceptT (Identity $ first show $ parse program "" s)