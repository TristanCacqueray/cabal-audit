module CabalAudit.Test.Instance where

import CabalAudit.Test.Class

data Tea = Tea

instance TestClass Tea where
  tasty = not . alwaysTrue

instance Show Tea where
  show Tea = "the" ++ "!"

alwaysTrue :: Tea -> Bool
alwaysTrue = const True
