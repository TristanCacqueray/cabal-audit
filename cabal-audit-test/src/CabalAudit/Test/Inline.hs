module CabalAudit.Test.Inline where

{-# INLINE fonctionInlined #-}
fonctionInlined :: Int -> Int
fonctionInlined x = x - 1
