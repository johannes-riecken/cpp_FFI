module Main where

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc

foreign import ccall "_ZN9MathFuncs11MyMathFuncs3AddEdd" add :: CDouble -> CDouble -> CDouble
foreign import ccall "_ZN9MathFuncs11MyMathFuncsC1Eii" ctor :: Ptr () -> CInt -> CInt -> IO ()
foreign import ccall "_ZN9MathFuncs11MyMathFuncs7InstAddEi" instAdd :: Ptr () -> CInt -> IO ()
foreign import ccall "_ZN9MathFuncs11MyMathFuncs4GetXEv" getX :: Ptr () -> IO CInt
main :: IO ()
main = do
  print $ add 3.1 5.4
  inst <- mallocBytes 8
  ctor inst 3 4
  instAdd inst 5
  x <- getX inst
  print x
