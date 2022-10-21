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

prop_find :: [CInt] -> Fun (CInt,CInt) CBool -> Property
prop_find xs (Fn2 p) = unsafePerformIO $ do
    let p' x y = if x == y then 1 else 0
    xs' <- newArray xs
    cmp <- mkCompare p
    pure $ find xs' (genericLength xs) cmp === my_find xs' (genericLength xs) cmp
foreign import ccall "hs_equal" equal :: Ptr CInt -> CInt -> Ptr CInt -> CInt -> CBool

foreign import ccall "hs_my_equal" my_equal :: Ptr CInt -> CInt -> Ptr CInt -> CInt -> CBool

prop_equal :: [CInt] -> Fun (CInt,CInt) CBool -> Property
prop_equal xs (Fn2 p) = unsafePerformIO $ do
    let p' x y = if x == y then 1 else 0
    xs' <- newArray xs
    cmp <- mkCompare p
    pure $ equal xs' (genericLength xs) cmp === my_equal xs' (genericLength xs) cmp
-- AUTOGEN END
B
!;

  is($out, $want, 'generateHaskell base case');
}

# generateCWrappers
# {
#     my $in = qq!A\nextern "C" {\nfoo\n};\n!;
#     open my $f_in, '<', \$in;
#     my $out = '';
#     open my $f_out, '>', \$out;
#     generate::generateCWrappers($f_in, $f_out, [['find', ['f', 'l']], ['equal', ['f', 'l', 'f']]]);
#     my $want = q!A
# extern "C" {
#     int hs_find(int *arr, int len, int (*comp)(int, int)) {
#       std::vector<int> v{};
#       for (int i = 0; i < len; i++) {
#         v.push_back(arr[i]);
#       }
#       return *std::find(v.begin(), v.end(), comp);
#     }

#     int hs_my_find(int *arr, int len, int (*comp)(int, int)) {
#       std::vector<int> v{};
#       for (int i = 0; i < len; i++) {
#         v.push_back(arr[i]);
#       }
#       return *my_find(v.begin(), v.end(), comp);
#     }

#     bool hs_equal(int *arr, int len, int (*comp)(int, int)) {
#       std::vector<int> v{};
#       for (int i = 0; i < len; i++) {
#         v.push_back(arr[i]);
#       }
#       return std::equal(v.begin(), v.end(), comp);
#     }

#     bool hs_my_equal(int *arr, int len, int (*comp)(int, int)) {
#       std::vector<int> v{};
#       for (int i = 0; i < len; i++) {
#         v.push_back(arr[i]);
#       }
#       return my_equal(v.begin(), v.end(), comp);
#     }

# };
# B
# !;
#     is($out, $want, 'generateCWrappers base case');
# }
done_testing();