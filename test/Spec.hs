import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Subdivide (getLevels, isPow2)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcTests]

qcTests = testGroup "(QuickCheck)" [
    QC.testProperty "len getLevels = mx * my" $
        \m@(mx, my)  n ->
            -- TODO: should use a custom generator
            (mx >= 2   && my >= 2 &&
             mx < 5000 && my < 5000 &&
             isPow2 n &&
             n > 1 && n < 2 * maximum m)
            QC.==>
                length (concat $ getLevels m n) == mx * my ]
