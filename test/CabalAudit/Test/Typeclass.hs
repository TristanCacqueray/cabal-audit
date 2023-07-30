module CabalAudit.Test.Typeclass where

class TestClass a where
  tasty :: a -> Bool
