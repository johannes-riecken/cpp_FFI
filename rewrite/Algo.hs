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

collapse = foldr (\x acc -> if Just x == listToMaybe acc then acc else x:acc) []

foreign import ccall "hs_is_sorted_until" is_sorted_until :: Ptr CInt -> CInt -> FunPtr Compare -> CInt

foreign import ccall "hs_my_is_sorted_until" my_is_sorted_until :: Ptr CInt -> CInt -> FunPtr Compare -> CInt

prop_is_sorted_until :: [CInt] -> Property
prop_is_sorted_until xs = unsafePerformIO $ do
    let xs'' = collapse xs
    xs' <- newArray xs''
    cmp <- mkCompare (fromIntegral . fromEnum .: (<))
    pure $ is_sorted_until xs' (genericLength xs'') cmp === my_is_sorted_until xs' (genericLength xs'') cmp

-- AUTOGEN BEGIN
-- AUTOGEN END

main :: IO ()
main = do
    quickCheck prop_is_sorted_until
