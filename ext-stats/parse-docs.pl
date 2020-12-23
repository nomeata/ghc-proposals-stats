#!/usr/bin/env perl

use strict;
use warnings;

my %category;
my %paths;
my %basenames;
my %versions;
my %titles;
my @order;

my $dir = $ARGV[0];
my @files = `find $dir -name "*.rst"`;
foreach my $file (@files) {
  chomp $file;
  my $basename = $1 if $file =~ m,/([^/]*).rst$,;
  my $path = substr $file, length $dir, (length $file) -4;
  my $input = '';
  open FILE ,'<', $file or die $!;
  while( <FILE> ) { $input .= $_ }
  close FILE;

  my $title = $1 if $input =~ m/^([^\n]*)\n====+/gms;
  # print STDERR "No title in $basename\n" unless $title;
  $titles{$basename} = $title;

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
    $paths{$1} = $path;
    $basenames{$1} = $basename;
  }
  while ($input =~
    m,^\.\. toctree::\n    :maxdepth: 1\n\n((?:    (?:.*/)?[a-zA-Z_0-9]+\n)+),gms) {
    my $elems = $1;
    while ($elems =~ m,    (?:.*/)?([a-zA-Z_0-9]+)\n,gm) {
      if ($title eq "Language extensions") {
        push @order, $1
      } else {
        $category{$1} = $title;
      }
    }
  }
}


open OUT, ">", "GHC2021/versions.csv";
print OUT "extension,since,path,category\n";
for my $ext (sort keys %versions) {
  my $cat = "Other";
  $cat = $titles{$basenames{$ext}}   if exists $titles{$basenames{$ext}};
  $cat = $category{$basenames{$ext}} if exists $category{$basenames{$ext}};
  print OUT "$ext,$versions{$ext},$paths{$ext},$cat\n";
}
close OUT;
open OUT, ">", "GHC2021/order.csv";
print OUT "category\n";
for my $path (@order) {
  my $cat = "Other";
  print OUT "$titles{$path}\n";
}
close OUT;
