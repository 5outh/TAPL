import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Control.Applicative
import Data.Maybe (fromMaybe)
import Control.Monad (forever)
import System.IO
import Data.Functor.Identity
import Text.Parsec.Prim(Parsec, ParsecT)
import System.Console.Haskeline
import Control.Monad.Trans(lift)

data Term = TTrue
          | TFalse
          | TIf Term Term Term
          | TZero
          | TSucc Term
          | TPred Term
          | TIsZero Term
            deriving (Eq, Show)

-- Lexer

type Token = (SourcePos, String)

-- Split the source string into a list of tokens stripping all the whitespace.
scan :: String -> Either ParseError [Token]
scan = parse toks "(unknown)"

-- A token is either a non-empty sequence of alphanumeric chars or one of the
-- parens (,), paired with its position in the source string.
tok :: Parsec String a Token
tok  = liftA2 (,) getPosition (many1 alphaNum <|> string "(" <|> string ")")

toks :: Parsec String a [Token]
toks = spaces *> many (tok <* spaces) <* eof

-- Parser

-- Analogous to 'char', but for streams of Tokens instead of streams of chars.
symb :: String -> GenParser Token () String
symb sym = (token showToken posToken testToken) <?> show sym
    where
      showToken (pos, tok) = show tok
      posToken  (pos, tok) = pos
      testToken (pos, tok) = if tok == sym then Just tok else Nothing

-- Constants
p_zero, p_true, p_false :: Parsec [Token] () Term
p_zero  = TZero  <$ symb "0"
p_true  = TTrue  <$ symb "true"
p_false = TFalse <$ symb "false"

-- Return a parser that parses a symbol key followed by a term and returns
-- the parsed term.
keyword :: String -> Parsec [Token] () Term
keyword key = symb key >> p_term

-- Statements
p_if, p_succ, p_pred, p_iszero :: Parsec [Token] () Term
p_if     = liftA3 TIf (keyword "if") (keyword "then") (keyword "else")
p_succ   = TSucc   <$> keyword "succ"
p_pred   = TPred   <$> keyword "pred"
p_iszero = TIsZero <$> keyword "iszero"

-- A term is either one of "if", "succ", "pred", "iszero", "true", "false",
-- "0" constructs, or it is a term enclosed in parenthesis.
p_term :: Parsec [Token] () Term
p_term = choice [ p_iszero
                , p_false
                , p_true
                , p_zero
                , p_succ
                , p_pred
                , p_if
                ] <|> between (symb "(") (symb ")") p_term

-- A program is a term followed by EOF.
p_program :: Parsec [Token] () Term
p_program = p_term <* eof

-- Evaluator

-- Check whether a term is a numeric value.
isNumericVal :: Term -> Bool
isNumericVal TZero     = True
isNumericVal (TSucc t) = isNumericVal t
isNumericVal _         = False

-- Check whether a term is a value.
isVal :: Term -> Bool
isVal TTrue              = True
isVal TFalse             = True
isVal t | isNumericVal t = True
isVal _                  = False

-- If some single-step evaluation rule applies to a term, do the reduction
-- and return Just the result, otherwise return Nothing.
eval1 :: Term -> Maybe Term
eval1 (TIf TTrue  c _)                     = Just c
eval1 (TIf TFalse _ a)                     = Just a
eval1 (TIf t c a)                          = (\t' -> TIf t' c a) <$> eval1 t
eval1 (TSucc t)                            = TSucc <$> eval1 t
eval1 (TPred TZero)                        = Just TZero
eval1 (TPred (TSucc t)) | isNumericVal t   = Just t
eval1 (TPred t)                            = TPred <$> eval1 t
eval1 (TIsZero TZero)                      = Just TTrue
eval1 (TIsZero (TSucc t)) | isNumericVal t = Just TFalse
eval1 (TIsZero t)                          = TIsZero <$> eval1 t
eval1 _                                    = Nothing

-- Apply single-step evaluator while there are applicable evaluation rules,
-- and return the result.
-- this is sexy
eval :: Term -> Term
eval t = fromMaybe t (eval <$> eval1 t)

-- Convert a numeric value into an integer and return Just that integer, or
-- return Nothing otherwise.
toInt :: Term -> Maybe Int
toInt TZero                      = Just 0
toInt (TSucc t) | isNumericVal t = succ <$> toInt t
toInt _                          = Nothing

-- Pretty-print a term.  Replace "numeric values" with their actual values.
ppTerm :: Term -> String
ppTerm t = case toInt t of
             Just n  -> show  n
             Nothing -> show' t
    where
      show' TZero       = "0"
      show' TTrue       = "true"
      show' TFalse      = "false"
      show' (TSucc t)   = "succ " ++ show' t
      show' (TPred t)   = "pred " ++ show' t
      show' (TIsZero t) = "iszero " ++ show' t
      show' (TIf t c a) = "if " ++ show' t ++ " then " ++ show' c ++ " else " ++ show' a

-- Interpreter

-- In the call to 'parse' below we need to reset the initial position,
-- which is (line 1, column 1), to the position of the first token (in
-- the input).  Otherwise if the first token causes a parse error, the
-- location of the error won't be reported correctly.
run :: String -> InputT IO ()
run input = 
  case scan input of 
    Left  err  -> outputStrLn $ "Parse error: " ++ show err
    Right toks -> case toks of
      []      -> return ()
      (tok:_) -> case parse ((setPosition . fst $ tok) >> p_program) "" toks of
                   Left  err  ->  outputStrLn "Parse error:" >> (lift $ print err)
                   Right term ->  outputStrLn . ppTerm $ eval term

loop :: InputT IO ()
loop = do
  minput <- getInputLine "> "
  case minput of
      Nothing -> return ()
      Just ":q" -> outputStrLn "Goodbye!" >> return ()
      Just input -> run input >> loop

main :: IO ()
main = runInputT defaultSettings loop
