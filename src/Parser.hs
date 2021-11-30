{-# LANGUAGE TupleSections #-}

module Parser (parseToken) where

import Rpn
import Value
import Operator
import Bases
import Error
import Util (fmap2, (<<$>>), splitOn, stripPrefixes, stripChar, stripEndChar)

import Text.Read
import Data.Ratio
import Data.List
import Data.Maybe
import Control.Monad


parseToken :: String -> Result Token
-- parse a token (wrapper for parseToken')
parseToken s = case parseToken' s of
                 Just r -> r                    -- use descriptive result
                 _      -> mkErr (TokenParseE s)  -- no parser matched, use generic error


-- a parser attempts to produce a value of type 'a' from a String
-- the outer Maybe describes whether the parser 'matched' the input
--   this determines whether another paser should be tried
-- the inner Result describes whether the string is valid in the context of the parser
--   this allows for descriptive error messages on parse failures, and short circuiting
-- e.g. parseValue "hello"   -> Nothing          : the parser did not match
--      parseValue "0bhello" -> Just (Err ...)   : the parser matched, but the string was invalid
--      parseValue "0b1001"  -> Just (Ok ...)    : the parser matched, and the string was valid
type Parser a = String -> Maybe (Result a)


-- parser utils

composeParsers :: [Parser a] -> Parser a
-- compose a list of parsers by trying each in order until one matches
composeParsers = foldl comp (const Nothing)
  where comp p p' s = case p s of
                        Just r  -> Just r
                        Nothing -> p' s

-- map from sets of strings to parse results
-- e.g. print, p -> CmdIOT Print  where a :: Token
type ParseMap a = [([String], a)]

parseFromMap :: ParseMap a -> Parser a
-- generate a generic parser from a ParseMap
parseFromMap pmap s = Ok <$> lookup s (expandMap pmap)
  where expandMap = concatMap (\(ss, x) -> map (, x) ss)

parseWithInt :: [String] -> (Int -> a) -> Parser a
-- generate a parser for a prefix followed by an integer (>= 1)
--   e.g. pop3 -> Pop 3  where a :: Command
parseWithInt prefixes handler s
  = Ok . handler <$> mfilter (>= 1) (readMaybe =<< stripPrefixes s prefixes)

parseWithStr :: String -> (String -> a) -> Parser a
-- generate a parser for a prefix followed by a non-empty string
parseWithStr prefix handler s = Ok . handler <$> mfilter (/= "") (stripPrefix prefix s)

-- parsers
parseToken' :: Parser Token
-- the master parser, composes each parser and raise their types to Token
parseToken' = composeParsers [operator, command, commandIO, value, jump, branch, parseFromMap m]  -- parsers to try (l to r)
  where operator  = fmap2 (TokenPure . OpT)  . parseOperator
        command   = fmap2 (TokenPure . CmdT) . parseCommand
        commandIO = fmap2 CmdPrintT          . parseCommandIO
        value     = fmap2 (TokenPure . ValT) . parseValue
        jump      = parseWithStr "J" (Jump False)
        branch    = parseWithStr "B" (Jump True)
        m = [(["RET"], RetT),
             (["ERR"], ErrT)]


parseOperator :: Parser Operator
parseOperator = parseFromMap m
  where m = [(["abs"]               , Op1 opAbs      ),
             (["neg", "negate"]     , Op1 opNegate   ),
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
             (["fl", "float"]       , Op1 opFloat    ),
             (["++"]                , OpF opAdd      ),
             (["**"]                , OpF opMultiply ),
             (["&&"]                , OpF opAnd      ),
             (["||"]                , OpF opOr       )]


parseCommand :: Parser Command
parseCommand = composeParsers [parseFromMap m, parsePop, parseDup, parsePull, parsePush, parseStore, parseLoad]
  where
    m = [(["pop", "r"]   , Pop 1  ),
         (["clear", "c"] , Clear  ),
         (["dup", "d"]   , Dup 1  ),
         (["swap", "s"]  , Push 1 ),  -- alias for push1
         (["depth", "z"] , Depth  )]
    parsePop    = parseWithInt ["pop", "r"]   Pop
    parseDup    = parseWithInt ["dup", "d"]   Dup
    parsePull   = parseWithInt ["pull", "pl"] Pull
    parsePush   = parseWithInt ["push", "ps"] Push
    parseStore  = parseWithStr "s"            Store
    parseLoad   = parseWithStr "l"            Load


parseCommandIO :: Parser CommandPrint
parseCommandIO = composeParsers [parseFromMap m, parsePrint, parseView]
  where
    m = [(["print", "p"] , Print Nothing ),
         (["stack", "f"] , Stack         ),
         (["view", "v"]  , ViewAll       )]

    -- p'n(c) : print in base n
    -- pb(c)  : print in base 2
    -- po(c)  : print in base 8
    -- px(c)  : print in base 16
    -- print in radix complement when 'c' is given
    parsePrint = composeParsers [parseBaseN, parseBin, parseOct, parseHex]
      where
        -- 'n form
        parseBaseN s = parseBaseN' <$> stripPrefix "p'" s
          where
            parseBaseN' form = do
              b  <- toResult (InvalidBaseSE s) (readMaybe s)
              assert (validBase b) (InvalidBaseE b)
              return $ Print (Just (b, compl))
              where
                (s, compl) = stripEndChar '~' form

        -- b/o/x form
        parseBaseChar b c s = Ok . makePrint <$> (checkCompl =<< stripPrefix ('p' : [c]) s)
          where
            makePrint compl = Print (Just (b, compl))
            checkCompl "~" = Just True   -- e.g. px~
            checkCompl ""  = Just False  -- e.g. px
            checkCompl _   = Nothing     -- e.g. pxhi
        parseBin = parseBaseChar 2  'b'
        parseOct = parseBaseChar 8  'o'
        parseHex = parseBaseChar 16 'x'

    parseView s = Ok . View <$> stripPrefix "v" s


parseValue = composeParsers [parseVI, parseVR, parseVF, parseVNeg, parseVBased, parseVConst]
  where
    -- n
    parseVI s = Ok . I <$> readMaybe s

    -- n/m
    parseVR s = reduceR <<$>> (buildR <$> splitOn '/' s)
      where buildR (s', s'') = do i'  <- toResult' s'  $ readMaybe s'
                                  i'' <- toResult' s'' $ readMaybe s''
                                  return $ i' % i''
              where toResult' = toResult . FracParseE

    -- n.
    parseVF f = Ok . F <$> readMaybe f

    -- -[val]
    parseVNeg s = negateVal <<$>> (parseValue =<< stripPrefix "-" s)

    -- n(~)'[int]     : base n literal
    -- (0)b(~)[int]   : base 2 literal
    -- (0)o(~)[int]   : base 8 literal
    -- (0)x(~)[int]   : base 16 literal
    -- literals are interpretted as radix complement form when 'c' is given
    parseVBased = fmap2 I . composeParsers [parseBaseN, parseBin, parseOct, parseHex]
      where
        -- n' form
        parseBaseN s = parseBaseN' <$> splitOn '\'' s
          where
            parseBaseN' (form, lit) = do
              b  <- toResult (InvalidBaseSE s) (readMaybe base)
              assert (validBase b) (InvalidBaseE b)
              parseB compl b lit
              where
                (base, compl) = stripEndChar '~' form

        -- b/o/x form
        parseBaseChar b c s = parseBaseChar' <$> stripPrefixes s ['0' : [c], [c]]
          where
            parseBaseChar' s = parseB compl b lit
              where
                (lit, compl) = stripChar '~' s
        parseBin = parseBaseChar 2  'b'
        parseOct = parseBaseChar 8  'o'
        parseHex = parseBaseChar 16 'x'

    parseVConst = parseFromMap m
      where m = [(["pi"] , F pi      ),
                 (["e"]  , F (exp 1) ),
                 (["g"]  , F 9.81    )]
