#!/bin/bash

# get latest version each
echo -n "Reading package list …"
pkgs_files=$(tar tzf ~/.cabal/packages/hackage.haskell.org/00-index.tar.gz | grep -v preferred-versions | sort -k1,1 -k2,2rV -t'/' --stable | sort -k1,1 -t'/' --stable --unique)

selected_pkgs=()
for pkg_file in $pkgs_files
do
  # if grep -q -e '[, ]containers\([, ]\|$\)' 01-index/$pkg_file
  # then
    pkg="$(echo "$pkg_file" | cut -d/ -f1,2|tr / -)"
    selected_pkgs+=("$pkg")
  #fi
done
echo " done (${#selected_pkgs[@]} packages)"

mkdir -p hackage/

# for debugging
# selected_pkgs=("${selected_pkgs[@]:0:200}")

for pkg in "${selected_pkgs[@]}"
do
  if [ ! -e "hackage/$pkg" ]
  then
    ( cd "hackage" && cabal unpack "$pkg" )
  fi
done
