import Data.List hiding (find)
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import Test.QuickCheck
import System.IO.Unsafe
import Data.Function.Pointless
import Data.Maybe (listToMaybe)

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

collapse :: Eq a => [a] -> [a]
collapse = foldr (\x acc -> if Just x == listToMaybe acc then acc else x:acc) []

-- AUTOGEN BEGIN
foreign import ccall "hs_is_sorted" is_sorted :: Ptr CInt -> CInt -> FunPtr Compare -> CBool

foreign import ccall "hs_my_is_sorted" my_is_sorted :: Ptr CInt -> CInt -> FunPtr Compare -> CBool

prop_is_sorted :: [CInt] -> Fun (CInt,CInt) CBool -> Property
prop_is_sorted xs (Fn2 p) = unsafePerformIO $ do
    xs' <- newArray xs
    cmp <- mkCompare p
    pure $ is_sorted xs' (genericLength xs) cmp === my_is_sorted xs' (genericLength xs) cmp

-- AUTOGEN END

main :: IO ()
main = do
    quickCheck prop_is_sorted
