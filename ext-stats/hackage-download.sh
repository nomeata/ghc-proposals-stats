#!/bin/bash

[ -e 00-index.tar.gz ] || wget hackage.haskell.org/00-index.tar.gz

# get latest version each
echo "Reading package list …"
pkgs_files=$(tar tzf 00-index.tar.gz | grep -v preferred-versions | sort -k1,1 -k2,2rV -t'/' --stable | sort -k1,1 -t'/' --stable --unique)

packages() {
  selected_pkgs=()
  for pkg_file in $pkgs_files
  do
    # if grep -q -e '[, ]containers\([, ]\|$\)' 01-index/$pkg_file
    # then
      pkg="$(echo "$pkg_file" | cut -d/ -f1,2|tr / -)"
      selected_pkgs+=("$pkg")
      echo $pkg
    #fi
  done
  # echo " done (${#selected_pkgs[@]} packages)"
}

mkdir -p hackage/

# for debugging
# selected_pkgs=("${selected_pkgs[@]:0:200}")

unpack() {
  pkg=$1
  if [ ! -e "hackage/$pkg" ]
  then
    ( cd "hackage" && cabal unpack "$pkg" )
  fi
}
export -f unpack

packages | parallel -j8 unpack
