import Text.Printf (printf)
import Control.Monad (forM_)
import Data.Char (isDigit)

main :: IO ()
main = do
    putStrLn "Enter width of multiplication table: "
    widthStr <- getLine
    putStrLn "Enter height of multiplication table: "
    heightStr <- getLine

    let width  = parseInput widthStr
        height = parseInput heightStr

    putStrLn ""
    printTable width height

parseInput :: String -> Int
parseInput s
    | null s            = 10
    | not (all isDigit s) = 10
    | val < 1           = 10 
    | otherwise         = val
    where val = read s :: Int

printTable :: Int -> Int -> IO ()
printTable w h = do
    let maxValue = w * h
    let maxDigits = length (show maxValue) + 1
    
    let table = [[i*j | j <- [1..w]] | i <- [1..h]]

    forM_ table $ \row -> do
        forM_ row $ \val ->
            printf "|%*d|" maxDigits val
        putStrLn ""