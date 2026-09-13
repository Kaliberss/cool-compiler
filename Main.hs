module Main where
import System.Environment (getArgs)
import Lexer (lexer)
import Parser (parseProgram)
import Text.Megaparsec (parse, eof)
import Text.Show.Pretty (pPrint)
filePipeline :: FilePath -> IO ()

filePipeline filepath = do
    code <- readFile filepath

    case lexer code of
        Left lexErr ->
            putStrLn $ "Erro no Lexer: " ++ show lexErr

        Right tokens ->
            case parse (parseProgram <* eof) filepath tokens of
                Left parseErr -> print parseErr
                Right ast     -> do
                    putStrLn $ "--- AST gerada:"
                    pPrint ast

main :: IO()

main = do
    args <- getArgs
    case args of
        [filename] -> filePipeline filename
        _ -> putStrLn "Uso: test <src.cl>"
