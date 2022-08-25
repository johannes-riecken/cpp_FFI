#!/usr/bin/perl -w
package generate;
use v5.30;
use Data::Dumper;
use FindBin qw($Bin);
use lib "$Bin";
use autodie;

my %derefs = (
  'adjacent_find' => '*',
  'is_sorted_until' => '*',
  'is_sorted' => '',
  'equal' => '',
  'find_if' => '*',
  'any_of' => '',
  'find' => '*',
);

my %ret_types = (
  'adjacent_find' => 'int',
  'is_sorted_until' => 'int',
  'is_sorted' => 'bool',
  'equal' => 'bool',
  'find_if' => 'int',
  'any_of' => 'bool',
  'find' => 'int',
);

sub generateProperties {
  my ($fn, $params) = @_;
  my $ret = '';
  for ('', 'my_') {
    $ret .= qq{foreign import ccall "hs_$_$fn" $_$fn :: } . toTypeSpec($params) . " -> " . derefRetTypeForHs($fn) . "\n";
    $ret .= "\n";
  }
  $ret .= "prop_$fn :: [CInt]" . (grep('comp', $params->@*) && ' -> Fun (CInt,CInt) CBool') . " -> Property\n";
  $ret .= "prop_$fn xs (Fn2 p) = unsafePerformIO \$ do\n";
  $ret .= "    let p' x y = if x == y then 1 else 0\n";
  $ret .= "    xs' <- newArray xs\n";
  $ret .= "    cmp <- mkCompare p\n";
  $ret .= "    pure \$ $fn xs' (genericLength xs) cmp === my_$fn xs' (genericLength xs) cmp\n";
  return $ret;
}

sub generateCWrapper {
  my ($fn, $params) = @_;
  my @ret;
  for ('', 'my_') {
    push @ret, "$ret_types{$fn} hs_$_$fn(int *arr, int len" . (grep('comp', $params->@*) && ', int (*comp)(int, int)') . ') {';
    push @ret, '  std::vector<int> v{};';
    push @ret, '  for (int i = 0; i < len; i++) {';
    push @ret, '    v.push_back(arr[i]);';
    push @ret, '  }';
    push @ret, "  return $derefs{$fn}@{[$_ || 'std::']}$fn(v.begin(), v.end()" . (grep('comp', $params->@*) && ', comp') . ');';
    push @ret, '}';
    push @ret, '';
  }
  return @ret;
}

sub derefRetTypeForHs {
  my ($fn) = @_;
  my $ret = $ret_types{$fn};
  $ret = substr($ret, '*' eq substr($ret, 0, 1));
  $ret =~s /.*/C\u$&/;
  return $ret;
}

sub toTypes {
  my ($params) = @_;
  my @types;
  for ($params->@*) {
    if ($_ eq 'f') {
      push @types, 'Ptr CInt', 'CInt';
    } elsif ($_ eq 'l') {
      # ignore
    } elsif ($_ eq 'comp') {
      push @types, 'FunPtr Compare';
    } elsif ($_ eq 'val') {
      push @types, 'CInt';
    }
  }
  return @types;
}

sub toTypeSpec {
  my ($params) = @_;
  return join ' -> ', toTypes($params);
}

print join "\n", generateCWrapper('adjacent_find', ['f', 'l', 'comp']);

1;
