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
     "inherits",
     "method",
     "call",
     "uncall",
     "construct",
     "destruct",
     "skip",
     "from",
     "do",
     "loop",
     "until",
     "int",
     "nil",
     "if",
     "then",
     "else",
     "fi",
     "local",
     "delocal",
     "new",
     "delete",
     "copy",
     "uncopy"]

--Operator precedence identical to C
operatorTable :: [[(String, BinOp)]]
operatorTable =
    [ [("*", Mul), ("/", Div), ("%", Mod)],
      [("+", Add), ("-", Sub)],
      [("<", Lt), ("<=", Lte), (">", Gt), (">=", Gte)],
      [("=", Eq), ("!=", Neq)],
      [("&", BitAnd)],
      [("^", Xor)],
      [("|", BitOr)],
      [("&&", And)],
      [("||", Or)] ]    

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

brackets :: Parser a -> Parser a
brackets = Token.brackets tokenParser

colon :: Parser String
colon = Token.colon tokenParser

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep tokenParser

typeName :: Parser TypeName
typeName = identifier
                    
arrayTypeName :: Parser (TypeName, Integer)
arrayTypeName = do x <- try typeName <|> string "int"
                   y <- brackets integer
                   return (x, y)

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
    <|> ModSub <$ symbol "-="
    <|> ModXor <$ symbol "^="

assign :: Parser Statement
assign = Assign <$> identifier <*> modOp <*> expression

swap :: Parser Statement
swap = Swap <$> identifier <* symbol "<=>" <*> identifier

conditional :: Parser Statement
conditional =
    reserved "if"
    >> Conditional
    <$> expression
    <* reserved "then"
    <*> block
    <* reserved "else"
    <*> block
    <* reserved "fi"
    <*> expression

loop :: Parser Statement
loop =
    reserved "from"
    >> Loop
    <$> expression
    <* reserved "do"
    <*> block
    <* reserved "loop"
    <*> block
    <* reserved "until"
    <*> expression

localCall :: Parser Statement
localCall =
    reserved "call"
    >> LocalCall
    <$> methodName
    <*> parens (commaSep identifier)

localUncall :: Parser Statement
localUncall =
    reserved "uncall"
    >> LocalUncall
    <$> methodName
    <*> parens (commaSep identifier)

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

localBlock :: Parser Statement
localBlock =
    reserved "local"
    >> LocalBlock
    <$> dataType
    <*> identifier
    <* symbol "="
    <*> expression
    <*> block
    <* reserved "delocal"
    <* dataType
    <* identifier
    <* symbol "="
    <*> expression
        
objectBlock :: Parser Statement
objectBlock =
    reserved "construct"
    >> ObjectBlock
    <$> typeName
    <*> identifier
    <*> block
    <* reserved "destruct"
    <* identifier

skip :: Parser Statement
skip = Skip <$ reserved "skip"

copyReference :: Parser Statement
copyReference = 
    reserved "copy"
    >> CopyReference
    <$> typeName
    <*> identifier
    <*> identifier 

unCopyReference :: Parser Statement
unCopyReference = 
    reserved "uncopy"
    >> UnCopyReference
    <$> typeName
    <*> identifier
    <*> identifier
    
arrayConstruction :: Parser Statement
arrayConstruction =
    reserved "new"
    >> ArrayConstruction
    <$> arrayTypeName 
    <*> identifier  
    
arrayDestruction :: Parser Statement
arrayDestruction =
    reserved "delete"
    >> ArrayDestruction
    <$> arrayTypeName
    <*> identifier    

statement :: Parser Statement
statement = try assign
        <|> swap
        <|> conditional
        <|> loop
        <|> try localCall
        <|> try localUncall
        <|> objectCall
        <|> objectUncall
        <|> localBlock
        <|> objectBlock 
        <|> try arrayConstruction <|> objectConstruction
        <|> arrayDestruction
        <|> objectDestruction
        <|> skip
        <|> copyReference
        <|> unCopyReference

block :: Parser [Statement]
block = many1 statement

{-- Top Level Parsers --}
dataType :: Parser DataType
dataType = try (IntegerArrayType <$ reserved "int" <* symbol "[" <* symbol "]")  
           <|> IntegerType <$ reserved "int"
       <|> try (ObjectArrayType <$> typeName <* symbol "[" <* symbol "]")
           <|> ObjectType <$> typeName
       

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
    <*> optionMaybe (reserved "inherits" >> typeName)
    <*> many variableDeclaration
    <*> many1 methodDeclaration

program :: Parser Program
program = spaces >> GProg <$> many1 classDeclaration <* eof

parseString :: String -> Except String Program
parseString s = ExceptT (Identity $ first show $ parse program "" s)
