module Main where

import System.Environment
import System.IO
import Data.Char
import Lexer (lexer, Token(..), LexErro(..), Posicao(..)) 
main :: IO ()

main = do
    args <- getArgs
    case args of
        [path] -> do
            content <- readFile path
            case lexer content of 
                Left (LexErro msg (Pos l c)) -> do
                    hPutStrLn stderr $ "Erro léxico na linha " ++ show l ++ ", coluna " ++ show c ++ " - " ++ msg 
                    fail ""

                Right tokens -> do 
                    mapM_ print tokens

        _ -> do 
            hPutStrLn stderr "uso: cool-lexer <src.cl>"
            fail ""
