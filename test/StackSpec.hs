module StackSpec where

import HW3
import Test.Hspec


{- Testing evalRPN -}
evalRPNSpec :: Spec
evalRPNSpec =  
    describe "HW3.evalRPN" $  do
        specify "evalRPN [Push 2.0, Push 1.0, DoOp MinusOp] [] = 1.0" $ 
            evalRPN [Push 2.0, Push 1.0, DoOp MinusOp] [] `shouldBe` 1.0


{- Testing toRPNopt -}
testExpression :: Expr
testExpression = BinOp (Num 1.0) MinusOp (BinOp (Num 2.0) PlusOp (Num 3.0))
testResult :: ([StackInstr], Integer)
testResult = ([Push 2.0, Push 3.0, DoOp PlusOp, Push 1.0, Swap, DoOp MinusOp], 2)


toRPNoptSpec :: Spec
toRPNoptSpec =  
    describe "HW3.toRPNopt" $  do
        specify ("toRPNopt " ++ show testExpression ++ "should be " ++ show testResult) $ 
            toRPNopt testExpression `shouldBe` testResult

        describe "when given an expression with optimal depth 3" $  
            it "should find the optimal depth of 3" $ 
               snd (toRPNopt depth3) `shouldBe` 3

        describe "when given an expression with optimal depth 4" $  
            it "should find the optimal depth of 4" $ 
               snd (toRPNopt depth4) `shouldBe` 4
               

spec :: Spec
spec = do 
    describe "evalRPN" evalRPNSpec
    describe "toRPNopt" toRPNoptSpec


main :: IO ()
main = hspec spec

