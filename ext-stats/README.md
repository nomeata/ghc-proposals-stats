GHC20xx data gathering and tallying scripts
===========================================

This directory contains a bunch of rather hacky scripts that I (nomeata) use to
produce the statistics about the extensions used to guide the committee
process, as well as tally the votes.

I run the `.hs` programs with `cabal run`.


In GHC2021/ exist the following files

 * `versions.csv`

   Produced with
   ```
   ./parse-docs.pl ~/build/haskell/ghc/docs/users_guide/**/*.rst > GHC2021/versions.csv
   ```
   with a recent check-out of GHC

 * `hackage-totals.csv` and `hackage-data.csv`

   Produced by downloading hackage packages with
   ```
   ./hackage-download.sh
   ```
   (careful, multiple GB!) and then running
   ```
   cabal run hackage-count -- ../../hackage/*/
   ```

 * Files with data from the survey: TBD

 * Files with votes: TBD

 * `result.csv` and `result.csv`

   Aggregated from the other files by running
   ```
   cabal run merge
   ```
