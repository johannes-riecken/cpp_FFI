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
foreign import ccall "hs_adjacent_find" adjacent_find :: Ptr CInt -> CInt -> FunPtr Compare -> CInt

foreign import ccall "hs_my_adjacent_find" my_adjacent_find :: Ptr CInt -> CInt -> FunPtr Compare -> CInt

prop_adjacent_find :: [CInt] -> Fun (CInt,CInt) CBool -> Property
prop_adjacent_find xs (Fn2 p) = unsafePerformIO $ do
    let p' x y = if x == y then 1 else 0
    xs' <- newArray xs
    cmp <- mkCompare p
    pure $ adjacent_find xs' (genericLength xs) cmp === my_adjacent_find xs' (genericLength xs) cmp

-- AUTOGEN END

main :: IO ()
main = do
    quickCheck prop_adjacent_find
