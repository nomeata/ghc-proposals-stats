#!/usr/bin/env perl

use strict;
use warnings;

my $input;
while(<>){
   $input .= $_;
}

my %exts;
while ($input =~
  m/^\.\. extension:: ([a-zA-Z0-0]*)\n(?:(?:(?:    .*)?\n)*?(?:    :since: (\d+(\.\d+)+).*\n))?/gm) {
  my $ver = $2 || "?";
  if (exists $exts{$1}) {
    print STDERR "WARN: duplicate extension: $1\n";
    if ($exts{$1} ne $ver) {
      print STDERR "ERR: different versions: $exts{$1} vs. $ver\n";
    }
  }
  $exts{$1} = $ver;
}

print "extension;since\n";
for my $ext (sort keys %exts) {
  print "$ext;$exts{$ext}\n";
}
