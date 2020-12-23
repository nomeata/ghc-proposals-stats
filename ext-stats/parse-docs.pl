#!/usr/bin/env perl

use strict;
use warnings;

my %category;
my %paths;
my %versions;

my $dir = $ARGV[0];
my @files = `find $dir -name "*.rst"`;
foreach my $file (@files) {
  chomp $file;
  my $input = '';
  open FILE ,'<', $file or die $!;
  while( <FILE> ) { $input .= $_ }
  close FILE;

  while ($input =~
    m/^\.\. extension:: ([a-zA-Z0-0]*)\n(?:(?:(?:    .*)?\n)*?(?:    :since: (\d+(\.\d+)+).*\n))?/gm) {
    my $ver = $2 || "?";
    if (exists $versions{$1}) {
      print STDERR "WARN: duplicate extension: $1\n";
      if ($versions{$1} ne $ver) {
        print STDERR "ERR: different versions: $versions{$1} vs. $ver\n";
      }
    }
    $versions{$1} = $ver;
    $paths{$1} = substr $file, length $dir;
  }
  while ($input =~
    m/^([^\n]*)\n====+.*\n\.\. toctree::\n    :maxdepth: 1\n\n((?:    [a-zA-Z_0-9]+\n)+)/gms) {
    my $title = $1;
    my $elems = $2;
    while ($elems =~ m/    ([a-zA-Z_0-9]+)\n/gm) {
      $category{$1} = $title;
    }
  }
}


print "extension,since,path,category\n";
for my $ext (sort keys %versions) {
  my $cat = "Other";
  $cat = $category{$1} if $paths{$ext} =~ m,/([^/]*).rst$, and exists $category{$1};
  print "$ext,$versions{$ext},$paths{$ext},$cat\n";
}
