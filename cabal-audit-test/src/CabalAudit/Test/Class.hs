module CabalAudit.Test.Class where

class TestClass a where
    tasty :: a -> Bool

class TestClass2 a where
    classFun1 :: a -> Bool
    classFun2 :: a -> String
