import Data.List hiding (find)
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import Test.QuickCheck
import System.IO.Unsafe

instance CoArbitrary CInt where
  coarbitrary = coarbitraryIntegral

instance Arbitrary CBool where
  arbitrary = chooseEnum (0,1)
  shrink 1 = [0]
  shrink 0 = []

type Compare = CInt -> CInt -> CBool
foreign import ccall "wrapper"
  mkCompare :: Compare -> IO (FunPtr Compare)

instance Function CInt where
  function = functionIntegral

-- AUTOGEN BEGIN
-- AUTOGEN END

main :: IO ()
main = do
    -- let xs = [1,1,2,3,0]
    let xs = []
    let p x y = if x == y then 1 else 0
    xs' <- newArray xs
    cmp <- mkCompare p
    print $ adjacent_find xs' (genericLength xs) cmp
    -- quickCheck prop_adjacent_find
