import Data.IORef
import Foreign.C
import Foreign.Ptr
import System.IO.Unsafe
import Test.QuickCheck

instance CoArbitrary CInt where
  coarbitrary = coarbitraryIntegral

instance Arbitrary CBool where
  arbitrary = chooseEnum (0,1)
  shrink 1 = [0]
  shrink 0 = []

instance Function CInt where
  function = functionIntegral

type Generator = IO CBool

foreign import ccall "wrapper" mkGenerator :: Generator -> IO (FunPtr Generator)

foreign import ccall "changes" changes :: FunPtr Generator -> IO CBool

type StateFn = CInt -> (CBool,CInt)

stateFnToIORef :: StateFn -> IORef CInt -> IO CBool
stateFnToIORef f s_ref = do
    s <- readIORef s_ref
    let (a,s') = f s
    writeIORef s_ref s'
    pure a

prop_changes :: Fun CInt (CBool,CInt) -> Property
prop_changes (Fn f) = unsafePerformIO (do
    x_ref <- newIORef 0
    f' <- mkGenerator $ stateFnToIORef f x_ref
    res <- changes f'
    pure $ collect res (total res))

main :: IO ()
main = quickCheck prop_changes
