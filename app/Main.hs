
import System.IO 
import Text.Read


-- ppp :: [(String, String)] -> String
-- ppp [] = []
-- ppp ((x,y):xs) =  x ++ ",  " ++ y ++ ppp xs

-- sss :: [String] -> String
-- sss [] = []
-- sss (x:xs) = x ++ ", " ++ sss xs 

promt :: IO ()
promt = do
    putStr "> "
    hFlush stdout
    linea <- getLine
    case words linea of
        [] -> promt
        (h:c)
            | h == "salir" -> putStrLn "adiosito"
            | otherwise -> putStrLn ("\"" ++ h ++ "\"" ++ " no se reconoce como comando")
            



main :: IO ()
main = do
    putStrLn("bienvenido a la aventura")
    promt



