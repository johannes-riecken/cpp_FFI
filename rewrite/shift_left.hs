foreign import ccall "hs_shift_left" shift_left :: Ptr CInt -> CInt -> CInt -> IO ()

foreign import ccall "hs_arr_shift_left" arr_shift_left :: Ptr CInt -> CInt -> CInt -> IO ()

prop_shift_left :: [CInt] -> Property
prop_shift_left xs = forAll (choose (0,genericLength xs - 1)) $ \x -> unsafePerformIO $ do
-- prop_shift_left xs x = unsafePerformIO $ do
    xs0 <- newArray xs
    xs1 <- newArray xs
    shift_left xs0 (genericLength xs) x
    arr_shift_left xs1 (genericLength xs) x
    xs0' <- peekArray (length xs) xs0
    xs1' <- peekArray (length xs) xs1
    pure $ xs0' === xs1'
