{-
-- EPITECH PROJECT, 2024
-- compressor
-- File description:
-- HandleError
-}

module HandleError (checkArgs, checkArgsNumber, checkContent) where

import System.IO.Error (catchIOError)
import System.Exit
import MyData (Content(..))

printUsage :: IO ()
printUsage = putStrLn "USAGE: ./imageCompressor -n N -l L -f F\n" >>
    putStrLn "\tN\tnumber of colors in the final image" >>
    putStrLn "\tL\tconvergence limit" >>
    putStrLn "\tF\tpath to the file containing the colors of the pixels"

isPositif :: Int -> Bool
isPositif x = x /= -1

checkPosition :: (Int, Int) -> Bool
checkPosition (a, b) = isPositif a || isPositif b

checkColor :: (Int, Int, Int) -> Bool
checkColor (a, b, c) = isPositif a || isPositif b || isPositif c

checkElement :: Content -> IO ()
checkElement element
    | checkPosition (pos element) == False =
        printError "The file format is incorrect."
    | checkColor (rgb element) == False =
        printError "The file format is incorrect."
    | otherwise = return ()

checkContent :: [Content] -> IO ()
checkContent [] = return ()
checkContent contents = mapM_ checkElement contents

printError :: String -> IO ()
printError str = putStrLn (str ++ "\n") >>
    printUsage >>
    exitWith(ExitFailure 84)

checkInt :: String -> IO ()
checkInt input = case reads input :: [(Int, String)] of
    [(_, "")] -> return ()
    _ -> printError "The '-n' need to be an integer."

checkFloat :: String -> IO ()
checkFloat input = case reads input :: [(Float, String)] of
    [(_, "")] -> return ()
    _ -> printError "The '-l' need to be a float."

checkFile :: String -> IO ()
checkFile filePath = 
    catchIOError
        (readFile filePath >> return ())
        (\e -> either
            (\_ -> printError $ "File '" ++ filePath ++ "' does not exist.")
            ioError (Left e))

checkArgsNumber :: [String] -> IO ()
checkArgsNumber args
    | length args /= 6 = printError "Number of arguments must be equal to 6."
    | otherwise = return ()

checkArgs :: [String] -> IO ()
checkArgs [] = return ()
checkArgs (arg:val:other) =
    sequence_ $
        case arg of
            "-n" -> [checkInt val, checkArgs other]
            "-l" -> [checkFloat val, checkArgs other]
            "-f" -> [checkFile val, checkArgs other]
            _ -> [printError "An option is not recognised."]
checkArgs _ = printError "Invalid argument format."
