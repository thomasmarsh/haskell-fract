import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Fractal


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcTests]

qcTests = testGroup "(QuickCheck)" []
