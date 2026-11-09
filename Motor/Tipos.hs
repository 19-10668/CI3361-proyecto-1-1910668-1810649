module Motor.Tipos (
    Comando (Comando, cmd, arg1)
) where

data Comando = Comando {
    cmd :: String,
    arg1 :: String
} deriving (Show, Eq)