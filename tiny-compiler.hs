-- stack exec ghci

{-# LANGUAGE ViewPatterns #-}

module TinyThreePassCompiler where

import Control.Applicative
import qualified Data.Map as M

data AST
    = Imm Int
    | Arg Int
    | Add AST AST
    | Sub AST AST
    | Mul AST AST
    | Div AST AST
    deriving (Eq, Show)

data Token
    = TChar Char
    | TInt Int
    | TStr String
    deriving (Eq, Show)

alpha, digit :: String
alpha = ['a'..'z'] ++ ['A'..'Z']
digit = ['0'..'9']

tokenize :: String -> [Token]
tokenize [] = []
tokenize xxs@(c:cs)
    | c `elem` "-+*/()[]" = TChar c : tokenize cs
    | not (null i) = TInt (read i) : tokenize is
    | not (null s) = TStr s : tokenize ss
    | otherwise = tokenize cs
    where
        (i, is) = span (`elem` digit) xxs
        (s, ss) = span (`elem` alpha) xxs

compile :: String -> [String]
compile = pass3 . pass2 . pass1

type Parser a = [Token] -> Maybe (a, [Token])

chainl1
    :: Parser (a -> a -> a)
    -> Parser a
    -> Parser a
chainl1 pOp pSub toks = do
    (e, toks0) <- pSub toks
    let
        go e1 toks1 =
            case pOp toks1 of
                Just (op, toks2) -> do
                    (e2, toks3) <- pSub toks2
                    go (e1 `op` e2) toks3
                Nothing ->
                    pure (e1, toks1)
    go e toks0

pMulDiv, pAddSub :: Parser (AST -> AST -> AST)
pMulDiv (TChar '*' : rest) = pure (Mul, rest)
pMulDiv (TChar '/' : rest) = pure (Div, rest)
pMulDiv _ = empty

pAddSub (TChar '+' : rest) = pure (Add, rest)
pAddSub (TChar '-' : rest) = pure (Sub, rest)
pAddSub _ = empty

pPrim :: M.Map String Int -> Parser AST
pPrim args (TStr ident : rest) = pure $ (Arg (args M.! ident), rest)
pPrim _ (TInt imm : rest) = pure $ (Imm imm, rest)
pPrim args (TChar '(' : rest) = do
    (ex, TChar ')' : toks0) <- pExpr args rest
    pure $ (ex, toks0)
pPrim _ _ = empty

pExpr :: M.Map String Int -> Parser AST
pExpr args = chainl1 pAddSub (chainl1 pMulDiv (pPrim args))

parseExpression :: M.Map String Int -> [Token] -> AST
parseExpression args tokens =
    case pExpr args tokens of
        Just (ast, []) -> ast
        _ -> error "parse error"

pass1 :: String -> AST
pass1 str =
    case span (/= TChar ']') (tokenize str) of
        (TChar '[' : ss, TChar ']' : exprPart) ->
          parseExpression argMap exprPart
          where
              getArgStr (TStr ident) = ident
              argPart = map getArgStr ss
              argMap = M.fromList (zip argPart [0..])

data Pass2Res
    = RImm Int
    | RAST AST

pass2 :: AST -> AST
pass2 z = back (go z)
    where
        back :: Pass2Res -> AST
        back (RAST ast) = ast
        back (RImm n) = Imm n

        go :: AST -> Pass2Res
        go (Imm n) = RImm n
        go (Add x y) = bin Add (+) (go x) (go y)
        go (Sub x y) = bin Sub (-) (go x) (go y)
        go (Mul x y) = bin Mul (*) (go x) (go y)
        go (Div x y) = bin Div div (go x) (go y)
        go (Arg n) = RAST (Arg n)

        bin
            :: (AST -> AST -> AST)
            -> (Int -> Int -> Int)
            -> Pass2Res -> Pass2Res -> Pass2Res
        bin _ makeImm (RImm x) (RImm y) = RImm $ makeImm x y
        bin makeAST _ x y = RAST $ makeAST (back x) (back y)

pass3 :: AST -> [String]
pass3 ast = go ast
    where
        go (Imm n) = ["IM " ++ show n, "PU"]
        go (Arg n) = ["AR " ++ show n, "PU"]
        go (Add x y) = bin "AD" (go x) (go y)
        go (Sub x y) = bin "SU" (go x) (go y)
        go (Mul x y) = bin "MU" (go x) (go y)
        go (Div x y) = bin "DI" (go x) (go y)
        
        bin op x y = x ++ y ++ words "PO SW PO" ++ [op, "PU"]