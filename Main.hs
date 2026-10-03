module Main where
import System.Environment (getArgs)
import Lexer (lexer)
import Parser (parseProgram)
import Semantic (semanticAnalysis)
import AST(Program(..))
import Text.Megaparsec (parse, eof)
import Text.Show.Pretty (pPrint)
import Utils(formatAllErrors)

filePipeline :: FilePath -> IO ()

filePipeline filepath = do
    code <- readFile filepath

    case lexer code of
        Left lexErr ->
            putStrLn $ "Erro no Lexer: " ++ show lexErr

        Right tokens ->
            case parse (parseProgram <* eof) filepath tokens of
                Left parseErr -> putStrLn $ formatAllErrors parseErr
                Right (Program ast)     -> do
                  case semanticAnalysis ast of
                    Left semanticErrors -> do
                        putStrLn "Erros semânticos:"
                        mapM_ (\err -> putStrLn ("-" ++ err)) semanticErrors
                    
                    Right _ -> 
                        putStrLn "Análise semântica completa! Programa válido."
                   

main :: IO()

main = do
    args <- getArgs
    case args of
        [filename] -> filePipeline filename
        _ -> putStrLn "Uso: test <src.cl>"
