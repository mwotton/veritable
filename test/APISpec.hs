{-# LANGUAGE OverloadedStrings #-}

module APISpec(spec) where

import Test.Hspec.QuickCheck
import Test.Hspec.ShouldBe
import Control.Applicative
import Data.ByteString(ByteString,pack)
import Test.QuickCheck hiding (property)
import Network.Veritable.API




spec :: Specs
spec = describe "LZ4" $ do
    it "can compress" $ 
      1 == 1
--    it "can roundtrip using high compression"
--      (property $ forAll string $
--       \s -> uncompress (compressHC s) == s)  
      
       
-- FIX really want a better random string algo
string :: Gen ByteString
string = do
   nums <- elements [1..10000]
   -- can't handle embedded nulls
   pack <$> vectorOf nums (elements $ map fromIntegral [0..127])
