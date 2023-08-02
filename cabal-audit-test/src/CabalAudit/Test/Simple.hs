module CabalAudit.Test.Simple where

afficheNombre :: Int -> IO ()
afficheNombre 0 = pure ()
afficheNombre n = putStrLn (show n)

maFonction :: String -> IO ()
maFonction = putStr
