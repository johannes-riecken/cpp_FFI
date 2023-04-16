{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LinearTypes #-}
import Prelude (pure)
import Prelude.Linear hiding (reverse)
import qualified Data.List as L hiding (find, sort, reverse)
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import Test.QuickCheck hiding (generate)
import System.IO.Unsafe
import Data.Function.Pointless
-- import Data.Maybe (listToMaybe)
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

hsShiftRight :: Consumable a => [a] -> Int -> [a]
hsShiftRight xs n = drop n xs ++ takeEnd n xs

hsShiftLeft :: Consumable a => [a] -> Int -> [a]
hsShiftLeft xs n = drop n xs ++ takeEnd n xs

-- hsShiftRight' xs n = foldr (\x acc ->

-- hsShiftLeft xs n = foldr (\x (i,acc) -> ) (0,xs)

collapsePred :: (Consumable a, Movable a, Eq a) => a %1 -> [a] %1 -> (Bool,Ur a,[a])
collapsePred x acc = go (move x) (move acc) where
    go :: (Consumable a, Eq a) => Ur a %1-> Ur [a] %1-> (Bool,Ur a,[a])
    go (Ur x') (Ur acc') = (Just x' == listToMaybe acc',Ur x',acc')

collapse :: (Movable a,Eq a) => [a] %1 -> [a]
collapse = foldr (\x acc -> go (collapsePred x acc)) [] where
    go :: (Bool,Ur a,[a]) %1-> [a]
    go (p,Ur x',acc') = (if p then id else (x':)) acc'

-- AUTOGEN BEGIN
foreign import ccall "hs_exclusive_scan" exclusive_scan :: Ptr CInt -> CInt -> Ptr CInt -> IO ()

foreign import ccall "hs_arr_exclusive_scan" arr_exclusive_scan :: Ptr CInt -> CInt -> Ptr CInt -> IO ()

prop_exclusive_scan :: [CInt] -> [CInt] -> Property
prop_exclusive_scan xs ys = unsafePerformIO $ do
    xs0 <- newArray xs
    xs1 <- newArray xs
    ys0 <- newArray ys
    ys1 <- newArray ys
    exclusive_scan xs0 (L.genericLength xs) ys0
    arr_exclusive_scan xs1 (L.genericLength xs) ys1
    let (Ur xsl,xsc1) = length xs
    xs0' <- peekArray xsl xs0
    xs1' <- peekArray xsl xs1
    pure $ xs0' === xs1'
    let (Ur ysl, ysc1) = length ys
    ys0' <- peekArray ysl ys0
    ys1' <- peekArray ysl ys1
    pure $ ys0' === ys1'

-- AUTOGEN END

main :: IO ()
main = do
    pure ()
    print $ collapse ([1,1,2,3,3,4,5] :: [Int])
    -- quickCheck prop_exclusive_scan
