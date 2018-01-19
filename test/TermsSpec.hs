module TermsSpec where

import Test.Hspec
import Terms
import qualified Data.Map as Map

epred :: Expr
epred = EAbs (EMatch "nat"
              (EVar 0)
              (Map.fromList
                [("O", (0, EConstr "O" [])),
                 ("S", (1, EVar 0))]))

spec :: Spec
spec = do
  describe "eval" $ do
    it "can evaluate simple expressions" $
      eval Map.empty (EBinOp Plus (EInt 3) (EInt 4)) `shouldBe` EInt 7

    it "can evaluate lambda applications" $
      eval (Map.fromList [("three", EInt 3)])
      (EApp (EAbs (EBinOp Plus (EName "three") (EVar 0))) (EInt 7))
      `shouldBe` EInt 10

    it "can evaluate matches" $
      eval Map.empty
      (EMatch
        "whatever"
        (EApp (EAbs (EConstr "c1" [(EInt 5), (EInt 6)])) (EInt 7))
        (Map.fromList [("c0", (2, EVar 1)), ("c1", (2, EVar 0))]))
      `shouldBe` EInt 6

    it "evaluates pred 0 correctly" $
      eval (Map.fromList [("pred", epred)])
      (EApp (EName "pred") (EConstr "O" []))
      `shouldBe` EConstr "O" []

    it "evaluates pred 1 correctly" $
      eval (Map.fromList [("pred", epred)])
      (EApp (EName "pred") (EConstr "S" [EConstr "O" []]))
      `shouldBe` EConstr "O" []

    it "evaluates pred 2 correctly" $
      eval (Map.fromList [("pred", epred)])
      (EApp (EName "pred") (EConstr "S" [EConstr "S" [EConstr "O" []]]))
      `shouldBe` EConstr "S" [EConstr "O" []]
