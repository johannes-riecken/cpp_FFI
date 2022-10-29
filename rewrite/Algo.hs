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

type UnaryPred = CInt -> CBool
foreign import ccall "wrapper"
    mkUnaryPred :: UnaryPred -> IO (FunPtr UnaryPred)

instance Function CInt where
  function = functionIntegral

collapse :: Eq a => [a] -> [a]
collapse = foldr (\x acc -> if Just x == listToMaybe acc then acc else x:acc) []

-- AUTOGEN BEGIN
foreign import ccall "hs_find_if" find_if :: Ptr CInt -> CInt -> FunPtr UnaryPred -> CInt

foreign import ccall "hs_my_find_if" my_find_if :: Ptr CInt -> CInt -> FunPtr UnaryPred -> CInt

prop_find_if :: [CInt] -> Fun CInt CBool -> Property
prop_find_if xs (Fn p) = unsafePerformIO $ do
    xs' <- newArray xs
    cmp <- mkUnaryPred p
    pure $ find_if xs' (genericLength xs) cmp === my_find_if xs' (genericLength xs) cmp

-- AUTOGEN END

main :: IO ()
main = do
    quickCheck prop_find_if
