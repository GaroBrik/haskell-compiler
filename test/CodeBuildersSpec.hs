module CodeBuildersSpec (
  spec
  ) where

import Test.Hspec
import Test.QuickCheck
import qualified ArbitraryGens as Arb
import CodeBuilders

spec :: Spec
spec = do
  describe "CodeBuilders.mkArithOp" $ do
    it "is equivalent to performing the op" $ property $
      forAll Arb.arith $ \left ->
      forAll arbitrary $ \op ->
      forAll Arb.arith $ \right ->
      transOp op (eval left) (eval right) == eval $ mkArithM left op right
