prop_partition_point :: [CInt] -> Fun CInt CBool -> Property
prop_partition_point xs (Fn p) = unsafePerformIO $ do
    xs' <- newArray . uncurry (++) . partition (toEnum . fromIntegral . p) $ xs
    cmp <- mkUnaryPred p
    pure $ partition_point xs' (genericLength xs) cmp === my_partition_point xs' (genericLength xs) cmp
