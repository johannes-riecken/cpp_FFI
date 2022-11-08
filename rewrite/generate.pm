#!/usr/bin/perl -w
package generate;
use v5.30;
use Data::Dumper;
use FindBin qw($Bin);
use lib "$Bin";
use autodie;
use List::Util qw(any first);
use warnings qw(FATAL);

my %derefs = (
  'adjacent_find' => '*',
  'all_of' => '',
  'any_of' => '',
  'equal' => '',
  'find' => '*',
  'find_if' => '*',
  'is_paritioned' => '',
  'is_sorted' => '',
  'is_sorted_until' => '*',
  'lower_bound' => '*',
  'none_of' => '',
  'partition_point' => '*',
  'upper_bound' => '*',
);

my %ret_types = (
  'adjacent_find' => 'int',
  'all_of' => 'bool',
  'any_of' => 'bool',
  'equal' => 'bool',
  'find' => 'int',
  'find_if' => 'int',
  'is_partitioned' => 'bool',
  'is_sorted' => 'bool',
  'is_sorted_until' => 'int',
  'lower_bound' => 'int',
  'none_of' => 'bool',
  'partition_point' => 'int',
  'replace' => 'void',
  'shift_left' => 'void',
  'shift_right' => 'void',
  'upper_bound' => 'int',
);

sub myPrefix {
  return $_[0] ? 'arr_' : 'my_';
}

sub isPredicate {
  my ($fn) = @_;
  return $fn eq 'comp' || $fn eq 'p';
}

my %pred_types = (
  'comp' => 'Compare',
  'p' => 'UnaryPred',
);

my %pred_qc_types = (
  'comp' => 'Fun (CInt,CInt) CBool',
  'p' => 'Fun CInt CBool',
);

sub predCTypes {
  my ($fn) = @_;
  my (undef, $param_types, $ret_type) = split / /, $pred_qc_types{$fn};
  if ('(' eq substr $param_types, 0, 1) {
    $param_types = substr $param_types, 1, -1;
  }
  my @param_types = split /,/, $param_types;
  my %hs_to_c = ('CInt' => 'int', 'CBool' => 'int');
  my @c_param_types = map { $hs_to_c{$_} } @param_types;
  my $c_ret_type = $hs_to_c{$ret_type};
  return "$c_ret_type (*$fn)(@{[join ', ', @c_param_types]})";
}

sub mkPred {
  return "mk\u$pred_types{$_[0]}"
}

sub predParamSuffix {
  my ($fn) = @_;
  return '' unless $fn;
  return ", @{[predCTypes($fn)]}[0]";
}

sub forAllSpec {
  return (grep { $_ eq 'i' } $_[0]->@*) ? 'forAll (choose (0,genericLength xs - 1)) $ \x -> ' : ''
}

sub generateProperties {
  my ($fn, $params, $is_arr) = @_;
  my $ret = '';
  for ('', $is_arr ? 'arr_' : 'my_') {
    $ret .= qq{foreign import ccall "hs_$_$fn" $_$fn :: } . toForeignTypeSpec($params) . " -> " . derefRetTypeForHs($fn) . "\n";
    $ret .= "\n";
  }
  my @types = toPropTypes($params);
  $ret .= "prop_$fn :: @{[toPropTypeSpec($params)]}\n";
  $ret .= "prop_$fn @{[toPropParamsStr(\@types)]} = ";
  $ret .= forAllSpec($params) . "unsafePerformIO \$ do\n";
  my @list_names = qw(xs ys);
  for ($params->@*) {
      if ($_ eq 'f') {
          my $name = shift @list_names;
          if ($is_arr) {
            $ret .= "    ${name}0 <- newArray $name\n";
            $ret .= "    ${name}1 <- newArray $name\n";
          } else {
            $ret .= "    $name' <- newArray $name\n";
          }
      } elsif (isPredicate($_)) {
          $ret .= "    cmp <- @{[mkPred($_)]} p\n";
      }
  }
  my $call_params = toCallParamsStr($params);
  if ($is_arr) {
    my $call_params0 = toCallParamsStr($params, 0);
    $ret .= "    $fn $call_params0\n";
    my $call_params1 = toCallParamsStr($params, 1);
    $ret .= "    arr_$fn $call_params1\n";
    $ret .= "    xs0' <- peekArray (length xs) xs0\n";
    $ret .= "    xs1' <- peekArray (length xs) xs1\n";
    $ret .= "    pure \$ xs0' === xs1'\n";
  } else {
      $ret .= "    pure \$ $fn $call_params === my_$fn $call_params\n";
  }
  $ret .= "\n";
  return $ret;
}

sub predicateParamSuffix {
  my ($params) = @_;
  return predParamSuffix(predicateArg($params));
}

sub predicateArg {
  my ($params) = @_;
  my $pred = first { isPredicate($_) } $params->@*;
  return $pred // '';
}

sub predicateArgSuffix {
  my ($params) = @_;
  my $pred = predicateArg($params);
  return $pred && ", $pred";
}

sub generateCWrapperRetVal {
  my ($fn, $prefix, $params, @fwd_args) = @_;
  my @ret;
  my %ret_names = ('*' => 'it', '' => 'ret');
  push @ret, "  auto $ret_names{$derefs{$fn}} = @{[$_ || 'std::']}$fn(@{[join ', ', @fwd_args]}" . predicateArgSuffix($params) . ((any { $_ eq 'val' } $params->@*) && ', val') . ');';
  if ($derefs{$fn}) {
      push @ret, '  return std::distance(arr0, it);';
  } else {
      push @ret, '  return ret;';
  }
  return @ret;
}

sub cleanHaskell {
  my ($f_in, $f_out) = @_;
  while (<$f_in>) {
    if (my $ff = $_ eq "-- AUTOGEN BEGIN\n" .. $_ eq "-- AUTOGEN END\n") {
      if ($ff > 1 && -1 == index $ff, 'E0') {
        $_ = '';
      }
    } elsif (my $ff2 = ($_ eq "main = do\n" .. !!0)) {
      if ($ff2 > 1) {
        $_ = '';
      }
    }
    print {$f_out} $_;
  }
  say {$f_out} '    pure ()';
}

sub cleanC {
  my ($f_in, $f_out) = @_;
  my $ff;
  while (<$f_in>) {
    if ($ff = $_ eq "\n" .. $_ eq "extern \"C\" {\n") {
      if ($ff > 1 && -1 == index $ff, 'E0') {
        $_ = '';
      }
    }
    print {$f_out} $_;
    last if $ff && (-1 != index $ff, 'E0');
  }
  while (<$f_in>) {
    if ($_ ne "};\n") {
      $_ = '';
    }
    print {$f_out} $_;
  }
}

sub valForC {
  my ($params) = @_;
  my $val = first { $_ eq 'val' || $_ eq 'i' } $params->@*;
  return $val && ", $val" || '';
}

sub valSuffixForC {
  my ($params) = @_;
  my $val = valForC($params);
  return $val =~ s/, /, int /r;
}

sub generateCWrapper {
  my ($fn, $params, $is_arr) = @_;
  my @ret;
  for ('', myPrefix($is_arr)) {
    my @args;
    my @fwd_args;
    my $n = grep { $_ eq 'f' } $params->@*;
    for (my $i = 0; $i < $n; $i++) {
        push @args, "int *arr$i", $i > 0 && $i == $n - 1 ? () : "int len$i";
        push @fwd_args, "arr$i", $i > 0 && $i == $n - 1 ? () : "arr$i + len$i";
    }
    push @ret, "$ret_types{$fn} hs_$_$fn(@{[join ', ', @args]}" . predicateParamSuffix($params) . valSuffixForC($params) . ') {';
    if ($is_arr) {
      push @ret, "  @{[$_ || 'std::']}$fn(@{[join ', ', @fwd_args]}" . predicateArgSuffix($params) . valForC($params) . ');';
    } else {
      push @ret, generateCWrapperRetVal($fn, $_, $params, @fwd_args);
    }
    push @ret, '}';
    push @ret, '';
  }
  return @ret;
}

sub derefRetTypeForHs {
  my ($fn) = @_;
  my $ret = $ret_types{$fn};
  if ($ret eq 'void') {
    return 'IO ()';
  }
  $ret = substr($ret, '*' eq substr($ret, 0, 1));
  $ret =~s /.*/C\u$&/;
  return $ret;
}

sub toTypes {
  my ($params) = @_;
  my @types;
  my $i = 0;
  my $n = grep { $_ eq 'f' } $params->@*;
  for ($params->@*) {
    if ($_ eq 'f') {
      push @types, 'Ptr CInt', $i > 0 && $i == $n - 1 ? () : 'CInt';
      $i++;
    } elsif ($_ eq 'l') {
      # ignore
    } elsif (isPredicate($_)) {
      push @types, "FunPtr $pred_types{$_}";
    } elsif ($_ eq 'val') {
      push @types, 'CInt';
    } elsif ($_ eq 'i') {
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

sub paramType {
  my ($fn) = @_;
  if (isPredicate($fn)) {
    return $pred_qc_types{$fn};
  }
  my %types = (
    'f' => '[CInt]',
    'val' => 'CInt',
  );
  return $types{$fn} // ();
}

sub toPropTypes {
    my ($params) = @_;
    my @types;
    for ($params->@*) {
        push @types, paramType($_);
    }
    push @types, 'Property';
    return @types;
}

sub toCallParams {
    my ($params, $idx) = @_;
    my @ret;
    my @list_names = qw(xs ys);
    my $n = grep { $_ eq 'f' } $params->@*;
    my $i = 0;
    for ($params->@*) {
        if ($_ eq 'f') {
            my $var = shift @list_names;
            if (defined $idx) {
              push @ret, "$var$idx";
            } else {
              push @ret, "$var'";
            }
            push @ret, $i > 0 && $i == $n - 1 ? () : "(genericLength $var)";
            $i++;
        } elsif (isPredicate($_)) {
            push @ret, 'cmp';
        } elsif ($_ eq 'val' || $_ eq 'i') {
            push @ret, 'x';
        }
    }
    return @ret;
}

sub toCallParamsStr {
    my ($params, $idx) = @_;
    return join ' ', toCallParams($params, $idx);
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
        } elsif ($_ eq 'Fun CInt CBool') {
            push @params, '(Fn p)';
        } elsif ($_ eq 'CInt') {
            push @params, 'x';
        }
    }
    return @params;
}

sub parseSignatures {
    my ($f_in, $is_arr) = @_;
    my @sigs;
    while (<$f_in>) {
      last if $_ eq qq!extern "C" {\n!;
      my $prefix = myPrefix($is_arr);
        if (/^auto ${prefix}(\w++)\(([^\(\)]*+)\) \{$/) {
            my $fn = $1;
            my $params_str = $2;
            my @params = map { s/^auto ([[:alpha:]]++).*+/$1/r } split ', ', $params_str;
            push @sigs, [$fn, \@params];
        }
    }
    return @sigs;
}

sub generateHaskell {
    my ($f_in, $f_out, $sigs, $is_arr) = @_;
    while (<$f_in>) {
        if (my $ff = $_ eq "-- AUTOGEN BEGIN\n" .. $_ eq "-- AUTOGEN END\n") {
            print {$f_out} $_ if $ff == 1 or 'E0' eq substr $ff, -2;
            if ($ff == 1) {
                for my $sig ($sigs->@*) {
                    print {$f_out} generateProperties($sig->[0], $sig->[1], $is_arr);
                }
            }
        } elsif ($_ eq "    pure ()\n") {
          for my $sig ($sigs->@*) {
            say {$f_out} "    quickCheck prop_$sig->[0]"
          }
        } else {
            print {$f_out} $_;
        }
    }
}

sub generateCWrappers {
    my ($f_in, $f_out, $sigs, $is_arr) = @_;
    while (<$f_in>) {
        if (my $ff = $_ eq qq!extern "C" {\n! .. $_ eq "};\n") {
            print {$f_out} $_ if $ff == 1 or 'E0' eq substr $ff, -2;
            if ($ff == 1) {
                for my $sig ($sigs->@*) {
                    if ($is_arr) {
                      print {$f_out} join '', map { "$_\n" =~ s/./    $&/r } generateCWrapper($sig->[0], $sig->[1], $is_arr);
                    } else {
                      print {$f_out} join '', map { "$_\n" =~ s/./    $&/r } generateCWrapper($sig->[0], $sig->[1], $is_arr);
                    }
                }
            }
        } else {
            print {$f_out} $_;
        }
    }
}

sub findImpl {
  my ($f_in, $fn) = @_;
  my $ret = '';
  while (<$f_in>) {
    if (my $ff = (0 == rindex $_, "auto $fn(", 0) .. $_ eq "\n") {
      if (-1 == index $ff, 'E0', -2) {
        $ret .= $_;
      } else {
        last;
      }
    }
  }
  return $ret;
}

sub insert {
  my ($f_db, $f_in, $f_out, $fn) = @_;
  my $impl = findImpl($f_db, $fn);
  my $done = !!0;
  while (<$f_in>) {
    if (!$done && $_ eq "\n") {
      $_ .= $impl . "\n";
      $done = !!1;
    }
    print {$f_out} $_;
  }
}

sub main {
    open my $f_cpp, '<', 'algorithm.cpp';
    my @sigs = parseSignatures($f_cpp);
    seek $f_cpp, 0, 0;
    my @arr_sigs = parseSignatures($f_cpp, !!1);
    close $f_cpp;
    if (@ARGV && $ARGV[0] eq '-h') {
        rename 'Algo.hs', 'Algo.hs.bak';
        open my $f_in, '<', 'Algo.hs.bak';
        open my $f_out, '>', 'Algo.hs';
        if (@arr_sigs) {
          generateHaskell($f_in, $f_out, \@arr_sigs, !!1);
        } else {
          generateHaskell($f_in, $f_out, \@sigs);
        }
        return;
    }

    if (@ARGV && $ARGV[0] eq '-c') {
      {
        rename 'Algo.hs', 'Algo.hs.bak';
        open my $f_in, '<', 'Algo.hs.bak';
        open my $f_out, '>', 'Algo.hs';
        cleanHaskell($f_in, $f_out);
      }
      {
        rename 'algorithm.cpp', 'algorithm.cpp.bak';
        open my $f_in, '<', 'algorithm.cpp.bak';
        open my $f_out, '>', 'algorithm.cpp';
        cleanC($f_in, $f_out);
      }
      return;
    }

    if (@ARGV == 2 && $ARGV[0] eq '-i') {
      open my $f_db, '<', 'algorithm.cpp.orig';
      rename 'algorithm.cpp', 'algorithm.cpp.bak';
      open my $f_in, '<', 'algorithm.cpp.bak';
      open my $f_out, '>', 'algorithm.cpp';
      insert($f_db, $f_in, $f_out, $ARGV[1]);
      return;
    }

    rename 'algorithm.cpp', 'algorithm.cpp.bak';
    open my $f_in, '<', 'algorithm.cpp.bak';
    open my $f_out, '>', 'algorithm.cpp';
    if (@arr_sigs) {
      generateCWrappers($f_in, $f_out, \@arr_sigs, !!1);
    } else {
      generateCWrappers($f_in, $f_out, \@sigs);
    }
}

main() unless caller;

1;
