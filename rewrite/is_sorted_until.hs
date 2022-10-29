prop_is_sorted_until :: [CInt] -> Property
prop_is_sorted_until xs = unsafePerformIO $ do
    let xs'' = collapse xs
    xs' <- newArray xs''
    cmp <- mkCompare (fromIntegral . fromEnum .: (<))
    pure $ is_sorted_until xs' (genericLength xs'') cmp === my_is_sorted_until xs' (genericLength xs'') cmp
