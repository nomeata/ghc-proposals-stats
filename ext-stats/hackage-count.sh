#!/bin/bash

[ -d "00-index" ] || aunpack  ~/.cabal/packages/hackage.haskell.org/00-index.tar.gz

# get latest version each
pkgs_files=$(find 00-index/ -type f -name \*.cabal |cut -d/ -f2-4 | sort -k1,1 -k2,2rV -t'/' --stable | sort -k1,1 -t'/' --stable --unique)

selected_pkgs=()
for pkg_file in $pkgs_files
do
  # if grep -q -e '[, ]containers\([, ]\|$\)' 01-index/$pkg_file
  # then
    pkg="$(echo "$pkg_file" | cut -d/ -f1,2|tr / -)"
    selected_pkgs+=("$pkg")
  #fi
done

mkdir -p hackage/

# for debugging
selected_pkgs=("${selected_pkgs[@]:0:200}")

tallies=()
failed_pkgs=()
skipped_pkgs=()
for pkg in "${selected_pkgs[@]}"
do
  echo -n "$pkg …"
  if [ ! -e "hackage/$pkg" ]
  then
    ( cd "hackage" && cabal unpack "$pkg" )
    echo -n " unpack …"
  fi

  if ( cd "hackage/$pkg" && extensions --union ) > "hackage/$pkg.extensions" 2> "hackage/$pkg.failure"
  then
    echo " good."
    tallies+=("hackage/$pkg.extensions")
  else
    echo " failed:"
    cat "hackage/$pkg.failure"
    failed_pkgs+=("$pkg")
  fi
done


sort /dev/null "${tallies[@]}" | uniq -c | sort -n > total.extensions
echo "Total  files:    ${#selected_pkgs[@]}"
echo "Parsed files:    ${#tallies[@]}"
echo "Failed to parse: ${#failed_pkgs[@]}"
echo "Skipped:         ${#skipped_pkgs[@]}"
