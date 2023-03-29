{-# LANGUAGE OverloadedStrings #-}

module Parser (parseInstructions) where

import Rpn
import Value
import Operator
import Bases
import Error
import Util ((.:))

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Ratio ((%))
import Data.Maybe (isJust, fromMaybe)
import Data.Tuple (swap)
import qualified Data.Map as M (lookup)
import Control.Monad (void)
import Control.Monad.Except
import Control.Monad.Reader


parseInstructions :: Defs -> Text -> Except Error.ParseError Instructions
-- parse a program source into a list of instructions
parseInstructions defs = liftEither . parse pInstructions' ""
  where pInstructions' = space *> runReaderT pInstructions defs <* eof


type Parser = ReaderT Defs (Parsec LogicParseError Text)


pInstructions :: Parser Instructions
pInstructions = many pInstruction
  where
    pInstruction = swap .: (,) <$> getOffset <*> pInstr


pInstr :: Parser Instr
pInstr = choice [Subroutine    <$> pSubroutineCall,
                 Operator      <$> pOperator,
                 Command       <$> pCommand,
                 CommandPrint  <$> pCommandPrint,
                 Value         <$> pValue,
                 uncurry If    <$> pIf,
                 uncurry While <$> pWhile,
                 Map           <$> pMap,
                 Fold          <$> pFold]


pValue :: Parser Value
pValue = signedVal (choice [pLit, pConst] <?> "value") <?> "value"
  where
    pLit :: Parser Value
    pLit = lexeme $ choice [F <$> pF, R <$> pR, I <$> pI]

    pI :: Parser Integer
    pI = withBase <|> default'
      where
        withBase = do
          ((base, compl), digits) <- (,) <$> pBase <*> some alphaNumChar
          case parseB compl base digits of
            Left  parseE -> customFailure parseE
            Right n      -> pure n
        default' = L.decimal

    pR :: Parser Rational
    pR = (%) <$> try (L.decimal <* char '/') <*> L.decimal

    pF :: Parser Double
    pF = try L.float

    pConst :: Parser Value
    pConst = parseFromMap m
      where m = [(["pi"] , F pi      ),
                 (["e"]  , F (exp 1) ),
                 (["g"]  , F 9.81    )]

    signedVal :: Parser Value -> Parser Value
    signedVal pVal = do
      neg <- parsePrefix (char '-')
      (if neg then negateVal else id) <$> pVal


pOperator :: Parser Operator
pOperator = parseFromMap m <?> "operator"
  where
    m = [(["abs", "||"]         , Op1 opAbs      ),
         (["negate", "neg"]     , Op1 opNegate   ),
         (["recip"]             , Op1 opRecip    ),
         (["sqrt", "root"]      , Op1 opSqrt     ),
         (["exp", "e^"]         , Op1 opExp      ),
         (["ln"]                , Op1 opLn       ),
         (["log2"]              , Op1 opLog2     ),
         (["fact", "!"]         , Op1 opFact     ),
         (["add", "plus", "+"]  , Op2 opAdd      ),
         (["sub", "minus", "-"] , Op2 opSubtract ),
         (["mul", "times", "*"] , Op2 opMultiply ),
         (["div", "/"]          , Op2 opDivide   ),
         (["idiv", "i/"]        , Op2 opIDivide  ),
         (["mod", "%"]          , Op2 opMod      ),
         (["pow", "^"]          , Op2 opPower    ),
         (["log"]               , Op2 opLog      ),
         (["ffact", "f!"]       , Op2 opFFact    ),
         (["rfact", "r!"]       , Op2 opRFact    ),
         (["sin"]               , Op1 opSin      ),
         (["cos"]               , Op1 opCos      ),
         (["tan"]               , Op1 opTan      ),
         (["sinh"]              , Op1 opSinh     ),
         (["cosh"]              , Op1 opCosh     ),
         (["tanh"]              , Op1 opTanh     ),
         (["asin", "arcsin"]    , Op1 opAsin     ),
         (["acos", "arccos"]    , Op1 opAcos     ),
         (["atan", "arctan"]    , Op1 opAtan     ),
         (["asinh", "arcsinh"]  , Op1 opAsinh    ),
         (["acosh", "arccosh"]  , Op1 opAcosh    ),
         (["atanh", "arctanh"]  , Op1 opAtanh    ),
         (["deg"]               , Op1 opDeg      ),
         (["rad"]               , Op1 opRad      ),
         (["not", "~"]          , Op1 opNot      ),
         (["and", "&"]          , Op2 opAnd      ),
         (["or", "|"]           , Op2 opOr       ),
         (["nand", "~&"]        , Op2 opNand     ),
         (["nor", "~|"]         , Op2 opNor      ),
         (["xor"]               , Op2 opXor      ),
         (["lshift", "<<"]      , Op2 opLShift   ),
         (["rshift", ">>"]      , Op2 opRShift   ),
         (["lt", "<"]           , Op2 opLt       ),
         (["lte", "<="]         , Op2 opLte      ),
         (["eq", "==", "="]     , Op2 opEq       ),
         (["neq", "!="]         , Op2 opNeq      ),
         (["gte", ">="]         , Op2 opGte      ),
         (["gt", ">"]           , Op2 opGt       ),
         (["rnd", "round"]      , Op1 opRnd      ),
         (["floor"]             , Op1 opFloor    ),
         (["ceil", "ceiling"]   , Op1 opCeil     ),
         (["fl", "float"]       , Op1 opFloat    )]


pCommand :: Parser Command
pCommand = choice [parseFromMap m, pPop, pDup, pPull, pPush, pStore, pLoad] <?> "command"
  where
    m = [(["clear", "c"] , Clear  ),
         (["swap" , "s"] , Push 1 ),
         (["depth", "z"] , Depth  )]
    pPop   = withIntOpt 1 ["pop"  , "r" ] Pop
    pDup   = withIntOpt 1 ["dup"  , "d" ] Dup
    pPull  = withInt      ["pull" , "pl"] Pull
    pPush  = withInt      ["push" , "ps"] Push
    pStore = withVar      ["store", "s" ] Store
    pLoad  = withVar      ["load" , "l" ] Load


pCommandPrint :: Parser CommandPrint
pCommandPrint = choice [pPrint, pStack, pView] <?> "command"
  where
    pPrint = choice [try withBase, default']
      where
        print    = ["print", "p"]
        withBase = Print . Just <$> lexeme (recognise print *> pBase)
        default' = parseItemLex print (Print Nothing)

    pStack = parseItemLex ["stack", "f"] Stack

    pView  = choice [try spec, default']
      where
        view     = ["view", "v"]
        spec     = withVar view (View . Just)
        default' = parseItemLex view (View Nothing)


pSubroutineCall :: Parser Instructions
pSubroutineCall = hidden . try $ do
  defs <- ask
  s <- lexeme pWord
  maybe empty return $ M.lookup (T.unpack s) defs
  where
    pWord = takeWhile1P Nothing (\c -> c /= ' ' && c /= ')')


pIf :: Parser (Instructions, Instructions)
pIf = recogniseLex ["If"] *>
      ((,) <$> pBlock <*> pBlock)


pWhile :: Parser (Instructions, Instructions)
pWhile = recogniseLex ["While"] *>
         ((,) <$> pBlock <*> pBlock)


pMap :: Parser Instructions
pMap = recogniseLex ["Map"] *> pBlock


pFold :: Parser Instructions
pFold = recogniseLex ["Fold"] *> pBlock


pBase :: Parser (Int, Bool)
pBase = choice [arb, spec]
  where
    arb = char '[' *> withCompl baseNum <* char ']'
    baseNum = do
      n <- L.decimal
      if validBase n then pure n
                     else customFailure (InvalidBaseE n)

    spec = withCompl (choice [bin, oct, hex])
    bin  = parseItem ["0b", "b"] 2
    oct  = parseItem ["0o", "o"] 8
    hex  = parseItem ["0x", "x"] 16

    withCompl :: Parser Int -> Parser (Int, Bool)
    withCompl pBase = (,) <$> pBase <*> compl
      where compl = isJust <$> optional (char '~')


pBlock :: Parser Instructions
pBlock = char '(' *> space *> pInstructions <* lexeme (char ')')


-- Parser utils

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (void (lookAhead (char ')')) <|> space1 <|> eof)

parsePrefix :: Parser a -> Parser Bool
parsePrefix = fmap isJust . optional

recognise :: [Text] -> Parser ()
recognise = void . choice . map string

recogniseLex :: [Text] -> Parser ()
recogniseLex = void . choice . map (try . lexeme . string)

parseItem :: [Text] -> a -> Parser a
parseItem ss x = pure x <* recognise ss

parseItemLex :: [Text] -> a -> Parser a
parseItemLex ss x = pure x <* recogniseLex ss

parseFromMap :: [([Text], a)] -> Parser a
parseFromMap = choice . map (uncurry parseItemLex)

withArg :: Parser b -> [Text] -> (b -> a) -> Parser a
withArg arg ss f = f <$> lexeme (recognise ss *> arg)

positiveDecimal :: Parser Int
positiveDecimal = do
  n <- L.decimal
  if n == 0 then customFailure ZeroIntArgE
            else pure n

withInt :: [Text] -> (Int -> a) -> Parser a
withInt = withArg positiveDecimal

withIntOpt :: Int -> [Text] -> (Int -> a) -> Parser a
withIntOpt default' = withArg (fromMaybe default' <$> optional positiveDecimal)

withVar :: [Text] -> (String -> a) -> Parser a
withVar ss f = lexeme (try (recognise ss *> char '[') *> (f <$> var) <* char ']')
  where
    var = some alphaNumChar <?> "variable name"

