module CabalAudit.Test.User where

import CabalAudit.Test.Class
import CabalAudit.Test.Instance
import CabalAudit.Test.Inline

useAlwaysTrue :: Bool
useAlwaysTrue = tasty Tea

monIncr :: (Num a) => a -> a
monIncr = (+) 1

useCofeeInstance :: Bool
useCofeeInstance = tasty Cofee

monDoubleDecr :: Int -> Int
monDoubleDecr x = fonctionInlined x - 1
