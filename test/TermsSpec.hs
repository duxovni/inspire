module TermsSpec where

import Test.Hspec
import Terms
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "eval" $ do
         it "can evaluate simple expressions" $
            eval Map.empty (EBinOp Plus (EInt 3) (EInt 4)) `shouldBe` EInt 7

         it "can evaluate lambda applications" $
            eval
            (Map.fromList [("three", EInt 3)])
            (EApp (EAbs (EBinOp Plus (EName "three") (EVar 0))) (EInt 7))
            `shouldBe` EInt 10

         it "can evaluate matches" $
            eval Map.empty
                   (EMatch
                    "whatever"
                    (EApp (EAbs (EConstr "c1" [(EInt 5), (EInt 6)])) (EInt 7))
                    (Map.fromList [("c0", EVar 1), ("c1", EVar 0)]))
                   `shouldBe` EInt 5
