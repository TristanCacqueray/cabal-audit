module CabalAudit.Test.User where

import CabalAudit.Test.Class
import CabalAudit.Test.Instance

useAlwaysTrue :: Bool
useAlwaysTrue = tasty Tea

monIncr :: Num a => a -> a
monIncr = (+) 1
