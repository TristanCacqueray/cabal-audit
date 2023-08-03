module CabalAudit.Test.Instance where

import CabalAudit.Test.Class

data Tea = Tea

instance TestClass Tea where
    tasty = not . alwaysTrue

instance Show Tea where
    show Tea = "the" ++ "!"

data Cofee = Cofee

instance TestClass Cofee where
    tasty _ = True && True

alwaysTrue :: Tea -> Bool
alwaysTrue = const True

instance TestClass2 Tea where
    classFun1 _ = True
    classFun2 = show

instance TestClass2 Cofee where
    classFun1 _ = False
    classFun2 _ = "Cafee"
