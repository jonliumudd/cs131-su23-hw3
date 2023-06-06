module ListsSpec where

import HW3
import Test.Hspec
import Test.QuickCheck


{- Testing evenodds -}
evenoddsSpec :: Spec
evenoddsSpec =  

    -- evenodds l == (evens l, odds l)
    describe "evenodds" $  
        specify "evenodds l == (evens l, odds l)" $ property $
            \l -> evenodds l == ( (evens l, odds l) :: ([Int], [Int]) )


{- Testing riffle -}
riffleSpec :: Spec
riffleSpec =  

    -- riffle should be the inverse of evenodds
    describe "riffle" $  
        specify "riffle (evenodds l) == l" $ property $
            \l -> riffle (evenodds l) == ( l :: [Int] )


spec :: Spec
spec = do 
    describe "evenodds" evenoddsSpec
    describe "riffle" riffleSpec


main :: IO ()
main = hspec spec

