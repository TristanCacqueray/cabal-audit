module CabalAudit.Test.External where

import Text.JSON qualified

decodeFloat :: String -> Maybe Float
decodeFloat str = case Text.JSON.decode str of
  Text.JSON.Ok v -> Just v
  Text.JSON.Error _ -> Nothing
