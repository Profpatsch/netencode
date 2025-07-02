{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import MyPrelude
import Netencode
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import Test.Hspec
import qualified Data.Map.NonEmpty as NEMap
import Data.List.NonEmpty (NonEmpty((:|)))

main :: IO ()
main = hspec $ do
  basicTypesTests
  compositeTypesTests  
  complexScenariosTests
  errorCasesTests

-- | Basic Types Tests
basicTypesTests :: Spec
basicTypesTests = describe "Basic Types" $ do
  it "unit value" $
      (netencodeEncodeStable unit & Builder.toLazyByteString & toStrictBytes) `shouldBe` "u,"
      
  it "natural zero" $
      (netencodeEncodeStable (natural 0) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "n:0,"
      
  it "natural number" $
      (netencodeEncodeStable (natural 42) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "n:42,"
      
  it "natural max u64" $
      (netencodeEncodeStable (natural 18446744073709551615) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "n:18446744073709551615,"
      
  it "integer zero" $
      (netencodeEncodeStable (integer 0) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "i:0,"
      
  it "integer positive" $
      (netencodeEncodeStable (integer 42) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "i:42,"
      
  it "integer negative" $
      (netencodeEncodeStable (integer (-42)) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "i:-42,"
      
  it "integer max i64" $
      (netencodeEncodeStable (integer 9223372036854775807) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "i:9223372036854775807,"
      
  it "integer min i64" $
      (netencodeEncodeStable (integer (-9223372036854775808)) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "i:-9223372036854775808,"
      
  it "boolean true" $
      (netencodeEncodeStable (boolean True) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "<4:true|u,"
      
  it "boolean false" $
      (netencodeEncodeStable (boolean False) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "<5:false|u,"
      
  it "text empty" $
      (netencodeEncodeStable (text "") & Builder.toLazyByteString & toStrictBytes) `shouldBe` "t0:,"
      
  it "text simple" $
      (netencodeEncodeStable (text "hello") & Builder.toLazyByteString & toStrictBytes) `shouldBe` "t5:hello,"
      
  it "text with space" $
      (netencodeEncodeStable (text "Hello, World!") & Builder.toLazyByteString & toStrictBytes) `shouldBe` "t13:Hello, World!,"
      
  it "text UTF-8 accented" $
      (netencodeEncodeStable (text "café") & Builder.toLazyByteString & toStrictBytes) `shouldBe` "t5:caf\xc3\xa9,"
      
  it "text UTF-8 emoji" $
      (netencodeEncodeStable (text "🌍") & Builder.toLazyByteString & toStrictBytes) `shouldBe` "t4:\xf0\x9f\x8c\x8d,"
      
  it "text with quotes" $
      (netencodeEncodeStable (text "He said \"hi\"") & Builder.toLazyByteString & toStrictBytes) `shouldBe` "t12:He said \"hi\","
      
  it "text with newline" $
      (netencodeEncodeStable (text "line1\nline2") & Builder.toLazyByteString & toStrictBytes) `shouldBe` "t11:line1\nline2,"
      
  it "binary empty" $
      (netencodeEncodeStable (binary "") & Builder.toLazyByteString & toStrictBytes) `shouldBe` "b0:,"
      
  it "binary simple" $
      (netencodeEncodeStable (binary "hello") & Builder.toLazyByteString & toStrictBytes) `shouldBe` "b5:hello,"
      
  it "binary with nulls" $
      (netencodeEncodeStable (binary "\0\1\2") & Builder.toLazyByteString & toStrictBytes) `shouldBe` "b3:\x00\x01\x02,"

-- | Composite Types Tests  
compositeTypesTests :: Spec
compositeTypesTests = describe "Composite Types" $ do
  it "tag simple" $
      (netencodeEncodeStable (tag "foo" unit) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "<3:foo|u,"
      
  it "tag empty name" $
      (netencodeEncodeStable (tag "" (integer 42)) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "<0:|i:42,"
      
  it "tag with value" $
      (netencodeEncodeStable (tag "Some" (text "value")) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "<4:Some|t5:value,"
      
  it "tag UTF-8" $
      (netencodeEncodeStable (tag "café" unit) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "<5:caf\xc3\xa9|u,"
      
  it "record single field" $ do
      let recordMap = NEMap.fromList (("a", unit) :| [])
      (netencodeEncodeStable (record recordMap) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "{7:<1:a|u,}"
      
  it "record two fields" $ do
      let recordMap = NEMap.fromList (("foo", integer 42) :| [("bar", text "baz")])
      (netencodeEncodeStable (record recordMap) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "{26:<3:bar|t3:baz,<3:foo|i:42,}"
      
  it "list empty" $
      (netencodeEncodeStable (list []) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "[0:]"
      
  it "list single item" $
      (netencodeEncodeStable (list [text "hello"]) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "[9:t5:hello,]"
      
  it "list multiple items" $ do
      let items = [text "foo", integer 42, unit]
      (netencodeEncodeStable (list items) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "[14:t3:foo,i:42,u,]"

-- | Complex Scenarios Tests
complexScenariosTests :: Spec  
complexScenariosTests = describe "Complex Scenarios" $ do
  it "nested list in record" $ do
      let nestedList = list [text "foo", text "bar"]
      let recordMap = NEMap.fromList (("items", nestedList) :| [])
      (netencodeEncodeStable (record recordMap) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "{28:<5:items|[14:t3:foo,t3:bar,]}"
      
  it "nested record in list" $ do
      let rec1 = record (NEMap.fromList (("x", natural 1) :| []))
      let rec2 = record (NEMap.fromList (("y", natural 2) :| []))
      (netencodeEncodeStable (list [rec1, rec2]) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "[26:{9:<1:x|n:1,}{9:<1:y|n:2,}]"
      
  it "unicode field names" $ do
      let recordMap = NEMap.fromList (("café", text "value") :| [])
      (netencodeEncodeStable (record recordMap) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "{18:<5:caf\xc3\xa9|t5:value,}"
      
  it "unicode complex" $
      (netencodeEncodeStable (text "Hello 世界 🌍") & Builder.toLazyByteString & toStrictBytes) `shouldBe` "t17:Hello \xe4\xb8\x96\xe7\x95\x8c \xf0\x9f\x8c\x8d,"
      
  it "unicode tag name" $
      (netencodeEncodeStable (tag "世界" unit) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "<6:\xe4\xb8\x96\xe7\x95\x8c|u,"
      
  it "text with null byte" $
      (netencodeEncodeStable (text "hello\0world") & Builder.toLazyByteString & toStrictBytes) `shouldBe` "t11:hello\x00world,"
      
  it "large binary" $ do
      let largeData = ByteString.replicate 1000 120  -- 1000 'x' bytes
      let result = netencodeEncodeStable (binary largeData) & Builder.toLazyByteString & toStrictBytes
      ByteString.isPrefixOf "b1000:" result `shouldBe` True
      ByteString.length result `shouldBe` 1007

-- | Type Safety Tests - demonstrating that Haskell's type system prevents errors
errorCasesTests :: Spec
errorCasesTests = describe "Type Safety" $ do
  it "natural values are inherently non-negative" $ do
      -- Word64 type prevents negative values at compile time
      (netencodeEncodeStable (natural 0) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "n:0,"
      
  it "natural values are inherently bounded to 64-bit" $ do
      -- Word64 type prevents overflow at compile time  
      (netencodeEncodeStable (natural maxBound) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "n:18446744073709551615,"
      
  it "integer values are inherently bounded to 64-bit signed" $ do
      -- Int64 type prevents overflow at compile time
      (netencodeEncodeStable (integer minBound) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "i:-9223372036854775808,"
      (netencodeEncodeStable (integer maxBound) & Builder.toLazyByteString & toStrictBytes) `shouldBe` "i:9223372036854775807,"
      
  it "text values are inherently valid UTF-8" $ do
      -- Text type guarantees valid UTF-8 encoding
      let result = netencodeEncodeStable (text "valid UTF-8: 🌍") & Builder.toLazyByteString & toStrictBytes
      ByteString.isPrefixOf "t" result `shouldBe` True
      
  it "binary values are inherently valid byte sequences" $ do
      -- ByteString type guarantees valid binary data
      let result = netencodeEncodeStable (binary "any bytes") & Builder.toLazyByteString & toStrictBytes
      ByteString.isPrefixOf "b" result `shouldBe` True