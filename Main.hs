module Main where

-- 1. Import your compiler phases
import Lexer (lexer)           -- (Assuming your Lexer exports a 'lexer' function)
import Parser (parseExpr, Parser(..))      -- The parser we just built

-- 2. Import Megaparsec for the test helper
import Text.Megaparsec (parse, eof)
import Text.Megaparsec.Error (errorBundlePretty)

-- | The test helper function we discussed
testParser :: Show a => Parser a -> String -> IO ()
testParser parser code = do
    -- Step 1: Lex the raw string into tokens
   case lexer code of
        Left lexErr ->
            putStrLn $ "Lexer Error: " ++ show lexErr

        Right tokens ->
            case parse (parser <* eof) "TestInput" tokens of
                Left parseErr -> print parseErr
                Right ast     -> print ast



-- | Run your tests here!
main :: IO ()
main = do
    putStrLn "--- Test 1: Math Precedence ---"
    testParser parseExpr "1 + 2 * 3"
    
    putStrLn "\n--- Test 2: Control Flow ---"
    testParser parseExpr "if true then 1 else 0 fi"
    
    putStrLn "\n--- Test 3: Chained Assignment ---"
    testParser parseExpr "x <- y <- 5"
    
    putStrLn "\n--- Test 4: Method Dispatch ---"
    testParser parseExpr "obj.methodName(1, 2)"
