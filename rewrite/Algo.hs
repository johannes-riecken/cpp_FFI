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

foreign import ccall "hs_is_sorted_until" is_sorted_until :: Ptr CInt -> CInt -> FunPtr Compare -> CInt

foreign import ccall "hs_my_is_sorted_until" my_is_sorted_until :: Ptr CInt -> CInt -> FunPtr Compare -> CInt

prop_is_sorted_until :: [CInt] -> Fun (CInt,CInt) CBool -> Property
prop_is_sorted_until xs (Fn2 p) = unsafePerformIO $ do
    let p' x y = if x == y then 1 else 0
    xs' <- newArray xs
    cmp <- mkCompare p
    pure $ is_sorted_until xs' (genericLength xs) cmp === my_is_sorted_until xs' (genericLength xs) cmp

foreign import ccall "hs_is_sorted" is_sorted :: Ptr CInt -> CInt -> FunPtr Compare -> CBool

foreign import ccall "hs_my_is_sorted" my_is_sorted :: Ptr CInt -> CInt -> FunPtr Compare -> CBool

prop_is_sorted :: [CInt] -> Fun (CInt,CInt) CBool -> Property
prop_is_sorted xs (Fn2 p) = unsafePerformIO $ do
    let p' x y = if x == y then 1 else 0
    xs' <- newArray xs
    cmp <- mkCompare p
    pure $ is_sorted xs' (genericLength xs) cmp === my_is_sorted xs' (genericLength xs) cmp

foreign import ccall "hs_equal" equal :: Ptr CInt -> CInt -> Ptr CInt -> CInt -> CBool

foreign import ccall "hs_my_equal" my_equal :: Ptr CInt -> CInt -> Ptr CInt -> CInt -> CBool

prop_equal :: [CInt] -> [CInt] -> Property
prop_equal xs ys = unsafePerformIO $ do
    let p' x y = if x == y then 1 else 0
    xs' <- newArray xs
    ys' <- newArray ys
    pure $ equal xs' (genericLength xs) ys' (genericLength ys) === my_equal xs' (genericLength xs) ys' (genericLength ys)

foreign import ccall "hs_find_if" find_if :: Ptr CInt -> CInt -> FunPtr Compare -> CInt

foreign import ccall "hs_my_find_if" my_find_if :: Ptr CInt -> CInt -> FunPtr Compare -> CInt

prop_find_if :: [CInt] -> Fun (CInt,CInt) CBool -> Property
prop_find_if xs (Fn2 p) = unsafePerformIO $ do
    let p' x y = if x == y then 1 else 0
    xs' <- newArray xs
    cmp <- mkCompare p
    pure $ find_if xs' (genericLength xs) cmp === my_find_if xs' (genericLength xs) cmp

foreign import ccall "hs_any_of" any_of :: Ptr CInt -> CInt -> FunPtr Compare -> CBool

foreign import ccall "hs_my_any_of" my_any_of :: Ptr CInt -> CInt -> FunPtr Compare -> CBool

prop_any_of :: [CInt] -> Fun (CInt,CInt) CBool -> Property
prop_any_of xs (Fn2 p) = unsafePerformIO $ do
    let p' x y = if x == y then 1 else 0
    xs' <- newArray xs
    cmp <- mkCompare p
    pure $ any_of xs' (genericLength xs) cmp === my_any_of xs' (genericLength xs) cmp

foreign import ccall "hs_find" find :: Ptr CInt -> CInt -> CInt -> CInt

foreign import ccall "hs_my_find" my_find :: Ptr CInt -> CInt -> CInt -> CInt

prop_find :: [CInt] -> CInt -> Property
prop_find xs x = unsafePerformIO $ do
    let p' x y = if x == y then 1 else 0
    xs' <- newArray xs
    pure $ find xs' (genericLength xs) x === my_find xs' (genericLength xs) x

-- AUTOGEN END

main :: IO ()
main = do
    let xs = [1,1,2,3,0]
    let p x y = if x == y then 1 else 0
    xs' <- newArray xs
    cmp <- mkCompare p
    print $ adjacent_find xs' (genericLength xs) cmp
    quickCheck prop_adjacent_find
