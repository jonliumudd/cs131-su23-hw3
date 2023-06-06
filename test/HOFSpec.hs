module HOFSpec where

import HW3
import Test.Hspec
import Test.QuickCheck


{- Testing myCurry -}
myCurrySpec :: Spec
myCurrySpec =  

    -- myCurry riffle should be the same as riffle2
    describe "myCurry"  $
        specify "myCurry riffle == riffle2" $ property $
            \l -> (let (es, os) = evenodds l in myCurry riffle es os) == (riffle (evenodds l) :: [Int])


{- Testing myUncurry -}
myUncurrySpec :: Spec
myUncurrySpec =  

    -- myUncurry riffle2 should be the same as riffle
    describe "myUncurry" $ 
        specify "myUncurry riffle2 == riffle" $ property $
            \l -> myUncurry (curry riffle) (evenodds l) == (riffle (evenodds l) :: [Int])


spec :: Spec
spec = do 
    describe "myCurry" myCurrySpec
    describe "myUncurry" myUncurrySpec


main :: IO ()
main = hspec spec