import Data.List hiding (find)
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import Test.QuickCheck hiding (generate)
import System.IO.Unsafe
import Data.Function.Pointless
import Data.Maybe (listToMaybe)
import Control.Monad (guard)
import Data.List.Extra (takeEnd)
import Data.IORef

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

type Generator = IO CInt
type StateFn = CInt -> (CInt,CInt)

stateFnToIORef :: StateFn -> IORef CInt -> IO CInt
stateFnToIORef f s_ref = do
    s <- readIORef s_ref
    let (a,s') = f s
    writeIORef s_ref s'
    pure a

foreign import ccall "wrapper"
    mkGenerator :: Generator -> IO (FunPtr Generator)

instance Function CInt where
  function = functionIntegral

hsShiftRight :: [a] -> Int -> [a]
hsShiftRight xs n = drop n xs ++ takeEnd n xs

hsShiftLeft :: [a] -> Int -> [a]
hsShiftLeft xs n = drop n xs ++ takeEnd n xs

-- hsShiftRight' xs n = foldr (\x acc ->

-- hsShiftLeft xs n = foldr (\x (i,acc) -> ) (0,xs)

collapse :: Eq a => [a] -> [a]
collapse = foldr (\x acc -> if Just x == listToMaybe acc then acc else x:acc) []

-- AUTOGEN BEGIN
-- AUTOGEN END

main :: IO ()
main = do
    pure ()
