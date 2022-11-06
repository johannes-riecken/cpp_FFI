#!/usr/bin/perl -w
package generate;
use v5.30;
use Data::Dumper;
use FindBin qw($Bin);
use lib "$Bin";
use generate;
use Test::More;
use autodie;

# parseSignatures
{
  my $in = qq!auto my_foo(auto bar, auto baz) {\nextern "C" {\n!;
  open my $f_in, '<', \$in;
  my @got = generate::parseSignatures($f_in);
  my @want = (['foo', ['bar', 'baz']]);
  is_deeply(\@got, \@want, 'parseSignatures base case');;
}
{
  my $in = qq!auto my_foo(auto f, auto l, auto f2, auto comp = std::less_equal{}) {\nextern "C" {\n!;
  open my $f_in, '<', \$in;
  my @got = generate::parseSignatures($f_in);
  my @want = (['foo', ['f', 'l', 'f', 'comp']]);
  is_deeply(\@got, \@want, 'parseSignatures f2');;
}

# parseArrSignatures
{
  my $in = qq!auto arr_foo(auto bar, auto baz) {\nextern "C" {\n!;
  open my $f_in, '<', \$in;
  my @got = generate::parseArrSignatures($f_in);
  my @want = ['foo', ['bar', 'baz']];
  is_deeply(\@got, \@want, 'parseArrSignatures base case');
}

# generateHaskell
{
  my $in = qq!A\n-- AUTOGEN BEGIN\nfoo\n-- AUTOGEN END\nB\n!;
  open my $f_in, '<', \$in;
  my $out = '';
  open my $f_out, '>', \$out;
  generate::generateHaskell($f_in, $f_out, [['find', ['f', 'l']], ['equal', ['f', 'l', 'f']]]);
  my $want = q!A
-- AUTOGEN BEGIN
foreign import ccall "hs_find" find :: Ptr CInt -> CInt -> CInt

foreign import ccall "hs_my_find" my_find :: Ptr CInt -> CInt -> CInt

prop_find :: [CInt] -> Property
prop_find xs = unsafePerformIO $ do
    xs' <- newArray xs
    pure $ find xs' (genericLength xs) === my_find xs' (genericLength xs)

foreign import ccall "hs_equal" equal :: Ptr CInt -> CInt -> Ptr CInt -> CBool

foreign import ccall "hs_my_equal" my_equal :: Ptr CInt -> CInt -> Ptr CInt -> CBool

prop_equal :: [CInt] -> [CInt] -> Property
prop_equal xs ys = unsafePerformIO $ do
    xs' <- newArray xs
    ys' <- newArray ys
    pure $ equal xs' (genericLength xs) ys' === my_equal xs' (genericLength xs) ys'

-- AUTOGEN END
B
!;

  is($out, $want, 'generateHaskell base case');
}
{
  my $in = qq!A\n-- AUTOGEN BEGIN\nfoo\n-- AUTOGEN END\nB\n!;
  open my $f_in, '<', \$in;
  my $out = '';
  open my $f_out, '>', \$out;
  generate::generateHaskell($f_in, $f_out, [['find',['f','l','val']]]);
  my $want = q!A
-- AUTOGEN BEGIN
foreign import ccall "hs_find" find :: Ptr CInt -> CInt -> CInt -> CInt

foreign import ccall "hs_my_find" my_find :: Ptr CInt -> CInt -> CInt -> CInt

prop_find :: [CInt] -> CInt -> Property
prop_find xs x = unsafePerformIO $ do
    xs' <- newArray xs
    pure $ find xs' (genericLength xs) x === my_find xs' (genericLength xs) x

-- AUTOGEN END
B
!;
  is($out, $want, 'generateHaskell val');
}
{
  my $in = qq!A\n-- AUTOGEN BEGIN\nfoo\n-- AUTOGEN END\nB\n!;
  open my $f_in, '<', \$in;
  my $out = '';
  open my $f_out, '>', \$out;
  generate::generateHaskell($f_in, $f_out, [['shift_left',['f','l','val']]], !!1);
  my $want = q!A
-- AUTOGEN BEGIN
foreign import ccall "hs_shift_left" shift_left :: Ptr CInt -> CInt -> CInt -> IO ()

foreign import ccall "hs_arr_shift_left" arr_shift_left :: Ptr CInt -> CInt -> CInt -> IO ()

prop_shift_left :: [CInt] -> CInt -> Property
prop_shift_left xs x = unsafePerformIO $ do
    xs0 <- newArray xs
    xs1 <- newArray xs
    shift_left xs0 (genericLength xs) x
    arr_shift_left xs1 (genericLength xs) x
    xs0' <- peekArray (length xs) xs0
    xs1' <- peekArray (length xs) xs1
    pure $ xs0' === xs1'

-- AUTOGEN END
B
!;
  is($out, $want, 'generateHaskell arr');
}
{
  my $in = qq!A\n-- AUTOGEN BEGIN\nfoo\n-- AUTOGEN END\nB\n!;
  open my $f_in, '<', \$in;
  my $out = '';
  open my $f_out, '>', \$out;
  generate::generateHaskell($f_in, $f_out, [['shift_left',['f','l','comp']]], !!1);
  my $want = q!A
-- AUTOGEN BEGIN
foreign import ccall "hs_shift_left" shift_left :: Ptr CInt -> CInt -> FunPtr Compare -> IO ()

foreign import ccall "hs_arr_shift_left" arr_shift_left :: Ptr CInt -> CInt -> FunPtr Compare -> IO ()

prop_shift_left :: [CInt] -> Fun (CInt,CInt) CBool -> Property
prop_shift_left xs (Fn2 p) = unsafePerformIO $ do
    xs0 <- newArray xs
    xs1 <- newArray xs
    cmp <- mkCompare p
    shift_left xs0 (genericLength xs) cmp
    arr_shift_left xs1 (genericLength xs) cmp
    xs0' <- peekArray (length xs) xs0
    xs1' <- peekArray (length xs) xs1
    pure $ xs0' === xs1'

-- AUTOGEN END
B
!;
  is($out, $want, 'generateHaskell arr p');
}
{
  my $in = qq!A\n-- AUTOGEN BEGIN\nfoo\n-- AUTOGEN END\nB\n!;
  open my $f_in, '<', \$in;
  my $out = '';
  open my $f_out, '>', \$out;
  generate::generateHaskell($f_in, $f_out, [['shift_left',['f','l','p']]], !!1);
  my $want = q!A
-- AUTOGEN BEGIN
foreign import ccall "hs_shift_left" shift_left :: Ptr CInt -> CInt -> FunPtr UnaryPred -> IO ()

foreign import ccall "hs_arr_shift_left" arr_shift_left :: Ptr CInt -> CInt -> FunPtr UnaryPred -> IO ()

prop_shift_left :: [CInt] -> Fun CInt CBool -> Property
prop_shift_left xs (Fn p) = unsafePerformIO $ do
    xs0 <- newArray xs
    xs1 <- newArray xs
    cmp <- mkUnaryPred p
    shift_left xs0 (genericLength xs) cmp
    arr_shift_left xs1 (genericLength xs) cmp
    xs0' <- peekArray (length xs) xs0
    xs1' <- peekArray (length xs) xs1
    pure $ xs0' === xs1'

-- AUTOGEN END
B
!;
  is($out, $want, 'generateHaskell arr p');
}

# generateCWrappers
{
    my $in = qq!A\nextern "C" {\nfoo\n};\n!;
    open my $f_in, '<', \$in;
    my $out = '';
    open my $f_out, '>', \$out;
    generate::generateCWrappers($f_in, $f_out, [['find', ['f', 'l']], ['equal', ['f', 'l', 'f']]]);
    my $want = q!A
extern "C" {
    int hs_find(int *arr0, int len0) {
      auto it = std::find(arr0, arr0 + len0);
      return std::distance(arr0, it);
    }

    int hs_my_find(int *arr0, int len0) {
      auto it = my_find(arr0, arr0 + len0);
      return std::distance(arr0, it);
    }

    bool hs_equal(int *arr0, int len0, int *arr1) {
      auto ret = std::equal(arr0, arr0 + len0, arr1);
      return ret;
    }

    bool hs_my_equal(int *arr0, int len0, int *arr1) {
      auto ret = my_equal(arr0, arr0 + len0, arr1);
      return ret;
    }

};
!;
    is($out, $want, 'generateCWrappers base case');
}
{
    my $in = qq!A\nextern "C" {\nfoo\n};\n!;
    open my $f_in, '<', \$in;
    my $out = '';
    open my $f_out, '>', \$out;
    generate::generateCWrappers($f_in, $f_out, [['shift_left', ['f', 'l', 'val']]], !!1);
    my $want = q!A
extern "C" {
    void hs_shift_left(int *arr0, int len0, int val) {
      std::shift_left(arr0, arr0 + len0, val);
    }

    void hs_arr_shift_left(int *arr0, int len0, int val) {
      arr_shift_left(arr0, arr0 + len0, val);
    }

};
!;
    is($out, $want, 'generateCWrappers arr');
}
{
    my $in = qq!A\nextern "C" {\nfoo\n};\n!;
    open my $f_in, '<', \$in;
    my $out = '';
    open my $f_out, '>', \$out;
    generate::generateCWrappers($f_in, $f_out, [['shift_left', ['f', 'l', 'comp']]], !!1);
    my $want = q!A
extern "C" {
    void hs_shift_left(int *arr0, int len0, int (*comp)(int, int)) {
      std::shift_left(arr0, arr0 + len0, comp);
    }

    void hs_arr_shift_left(int *arr0, int len0, int (*comp)(int, int)) {
      arr_shift_left(arr0, arr0 + len0, comp);
    }

};
!;
    is($out, $want, 'generateCWrappers arr comp');
}
{
    my $in = qq!A\nextern "C" {\nfoo\n};\n!;
    open my $f_in, '<', \$in;
    my $out = '';
    open my $f_out, '>', \$out;
    generate::generateCWrappers($f_in, $f_out, [['shift_left', ['f', 'l', 'p']]], !!1);
    my $want = q!A
extern "C" {
    void hs_shift_left(int *arr0, int len0, int (*p)(int)) {
      std::shift_left(arr0, arr0 + len0, p);
    }

    void hs_arr_shift_left(int *arr0, int len0, int (*p)(int)) {
      arr_shift_left(arr0, arr0 + len0, p);
    }

};
!;
    is($out, $want, 'generateCWrappers arr p');
}
{
    my $in = qq!A\nextern "C" {\nfoo\n};\n!;
    open my $f_in, '<', \$in;
    my $out = '';
    open my $f_out, '>', \$out;
    generate::generateCWrappers($f_in, $f_out, [['shift_left', ['f', 'l', 'f']]], !!1);
    my $want = q!A
extern "C" {
    void hs_shift_left(int *arr0, int len0, int *arr1) {
      std::shift_left(arr0, arr0 + len0, arr1);
    }

    void hs_arr_shift_left(int *arr0, int len0, int *arr1) {
      arr_shift_left(arr0, arr0 + len0, arr1);
    }

};
!;
    is($out, $want, 'generateCWrappers arr xs ys');
}

# cleanHaskell
{
  my $in = q!A
-- AUTOGEN BEGIN
foo
bar
-- AUTOGEN END
main :: IO ()
main = do
    quickCheck foo
    quickCheck bar
!;
    open my $f_in, '<', \$in;
    my $out = '';
    open my $f_out, '>', \$out;
    generate::cleanHaskell($f_in, $f_out);
    my $want = q!A
-- AUTOGEN BEGIN
-- AUTOGEN END
main :: IO ()
main = do
    pure ()
!;
  is($out, $want, 'cleanHaskell base case');
}

# cleanC
{
  my $in = q!#include <algorithm>
#include <numeric>

auto my_equal(auto f, auto l) {
    return 1;
}

extern "C" {
    int hs_my_equal() {
    }

    int my_equal() {
    }
};
!;
    open my $f_in, '<', \$in;
    my $out = '';
    open my $f_out, '>', \$out;
    generate::cleanC($f_in, $f_out);
    my $want = q!#include <algorithm>
#include <numeric>

extern "C" {
};
!;
    is($out, $want, 'cleanC base case');
}

done_testing();
