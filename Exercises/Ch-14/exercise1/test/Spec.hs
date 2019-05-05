import Test.Hspec

mySum :: (Eq a, Num a) => a -> a
mySum 0 = 0
mySum n = n + mySum (n - 1)

main :: IO ()
main = hspec $ do 
    describe "my Summation function" $ do 
        it "mySum 3 is 6" $ do 
            mySum 3 `shouldBe` 6
        it "mySum 4 is 10" $ do 
            mySum 4 `shouldBe` 10
        it "mySum 5 is 15" $ do
            mySum 5 `shouldBe` 15