module CIS194L01Spec where

import Courses.CIS194.L01
import Test.Hspec

spec :: Spec
spec = do
  describe "L01 - lastDigit" $ do
    it "lastDigit 0 should be 0" $ lastDigit 0 `shouldBe` 0
    it "lastDigit 1 should be 1" $ lastDigit 1 `shouldBe` 1
    it "lastDigit 12 should be 2" $ lastDigit 12 `shouldBe` 2
    it "lastDigit 123 should be 3" $ lastDigit 123 `shouldBe` 3
  describe "L01 - dropLastDigit" $ do
    it "dropLastDigit 0 should be 0" $ dropLastDigit 0 `shouldBe` 0
    it "dropLastDigit 1 should be 0" $ dropLastDigit 1 `shouldBe` 0
    it "dropLastDigit 12 should be 1" $ dropLastDigit 12 `shouldBe` 1
    it "dropLastDigit 123 should be 12" $ dropLastDigit 123 `shouldBe` 12
  describe "L01 - toRevDigits" $ do
    it "toRevDigits 0 should be []" $ toRevDigits 0 `shouldBe` []
    it "toRevDigits 1 should be [1]" $ toRevDigits 1 `shouldBe` [1]
    it "toRevDigits 1234 should be [4,3,2,1]" $ toRevDigits 1234 `shouldBe` [4,3,2,1]
    it "toRevDigits (-1) should be []" $ toRevDigits (-1) `shouldBe` []
  describe "L01 - doubleEveryOther" $ do
    it "doubleEveryOther [4,9,5,5] should be [4,18,5,10]" $ doubleEveryOther [4,9,5,5] `shouldBe` [4,18,5,10]
    it "doubleEveryOther [0,0] should be [0,0]" $ doubleEveryOther [0,0] `shouldBe` [0,0]
  describe "L01 - sumDigits" $ do
    it "sumDigits [10, 5, 18, 4] should be 19" $ sumDigits [10,5,18,4] `shouldBe` 19
  describe "L01 - luhn" $ do
    it "luhn 5594589764218858 should be True" $ luhn 5594589764218858 `shouldBe` True
    it "luhn 1234567898765432 should be False" $ luhn 1234567898765432 `shouldBe` False
--  describe "L01 - hanoi" $ do
--    it "hanoi 2 \"a\" \"b\" \"c\" should be [(\"a\",\"c\"), (\"a\",\"b\"), (\"c\",\"b\")]" $ hanoi 2 "a" "b" "c" `shouldBe` [("a","c"), ("a","b"), ("c","b")]
