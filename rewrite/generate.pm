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
    $ret .= qq{foreign import ccall "hs_$_$fn" $_$fn :: } . toForeignTypeSpec($params) . " -> " . derefRetTypeForHs($fn) . "\n";
    $ret .= "\n";
  }
  my @types = toPropTypes($params);
  $ret .= "prop_$fn :: @{[toPropTypeSpec($params)]}\n";
  $ret .= "prop_$fn @{[toPropParamsStr(\@types)]} = unsafePerformIO \$ do\n";
  $ret .= "    let p' x y = if x == y then 1 else 0\n";
  my @list_names = qw(xs ys);
  for ($params->@*) {
      if ($_ eq 'f') {
          my $name = shift @list_names;
          $ret .= "    $name' <- newArray $name\n";
      } elsif ($_ eq 'comp') {
          $ret .= "    cmp <- mkCompare p\n";
      }
  }
  my $call_params = toCallParamsStr($params);
  $ret .= "    pure \$ $fn $call_params === my_$fn $call_params\n";
  $ret .= "\n";
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

sub toForeignTypeSpec {
  my ($params) = @_;
  return join ' -> ', toTypes($params);
}

sub toPropTypeSpec {
    my ($params) = @_;
    return join ' -> ', toPropTypes($params);
}

sub toPropTypes {
    my ($params) = @_;
    my @types;
    for ($params->@*) {
        if ($_ eq 'f') {
            push @types, '[CInt]';
        } elsif ($_ eq 'comp') {
            push @types, 'Fun (CInt,CInt) CBool';
        } elsif ($_ eq 'val') {
            push @types, 'CInt';
        }
    }
    push @types, 'Property';
    return @types;
}

sub toCallParams {
    my ($params) = @_;
    my @ret;
    my @list_names = qw(xs ys);
    for ($params->@*) {
        if ($_ eq 'f') {
            my $var = shift @list_names;
            push @ret, "$var'";
            push @ret, "(genericLength $var)";
        } elsif ($_ eq 'comp') {
            push @ret, 'cmp';
        } elsif ($_ eq 'val') {
            push @ret, 'x';
        }
    }
    return @ret;
}

sub toCallParamsStr {
    my ($params) = @_;
    return join ' ', toCallParams($params);
}

sub toPropParamsStr {
    my ($types) = @_;
    return join ' ', toPropParams($types);
}

sub toPropParams {
    my ($types) = @_;
    my @list_names = qw(xs ys);
    my @params;
    for ($types->@*) {
        if ($_ eq '[CInt]') {
            push @params, shift @list_names;
        } elsif ($_ eq 'Fun (CInt,CInt) CBool') {
            push @params, '(Fn2 p)';
        } elsif ($_ eq 'CInt') {
            push @params, 'x';
        }
    }
    return @params;
}

sub parseSignatures {
    my ($f_in) = @_;
    my @sigs;
    while (<$f_in>) {
        if (!!1 .. $_ eq qq!extern "C" {\n!) {
            if (/^auto my_(\w++)\(([^\(\)]*+)\) \{$/) {
                my $fn = $1;
                my $params_str = $2;
                my @params = map { s/^auto ([[:alpha:]]++).*+/$1/r } split ', ', $params_str;
                push @sigs, [$fn, \@params];
            }
        }
    }
    return @sigs;
}

sub generateHaskell {
    my ($f_in, $f_out, $sigs) = @_;
    while (<$f_in>) {
        # if (my $ff = $_ eq "-- AUTOGEN BEGIN\n" .. $_ eq "-- AUTOGEN END\n") {
        if (my $ff = $_ eq "-- AUTOGEN BEGIN\n" .. $_ eq "-- AUTOGEN END\n") {
            print {$f_out} $_ if $ff == 1 or 'E0' eq substr $ff, -2;
            if ($ff == 1) {
                for my $sig ($sigs->@*) {
                    print {$f_out} generateProperties($sig->[0], $sig->[1]);
                }
            }
        } else {
            print {$f_out} $_;
        }
    }
}

sub main {
    open my $f_cpp, '<', 'algorithm.cpp';
    my @sigs = parseSignatures($f_cpp);
    rename 'Algo.hs', 'Algo.hs.bak';
    open my $f_in, '<', 'Algo.hs.bak';
    open my $f_out, '>', 'Algo.hs';
    generateHaskell($f_in, $f_out, \@sigs);
    # print join "\n", generateCWrapper('adjacent_find', ['f', 'l', 'comp']);
}

main() unless caller;

1;
