## Test environments

* Windows 11 x64 (local), R 4.6.1
* Windows 11 x64 (local), R 4.3.3


## R CMD check results

0 errors | 0 warnings | 0 notes on R 4.6.1

0 errors | 0 warnings | 1 note on R 4.3.3

* checking data for non-ASCII characters ... NOTE
  found 9 marked UTF-8 strings

## Comments

The R 4.3.3 note is locale/toolchain-specific. The marked strings are
intentional UTF-8 reference-library metadata. They
preserve contributor names, organization names, scientific units/symbols, and
the German color term "dunkelgrün". Regression tests cover the two corrected
mojibaked metadata values.

The two `read_opus()` warning failures reported for OpenSpecy 1.5.3 on CRAN
r-devel are fixed by aligning the block-end vector before logical subsetting.
Focused single-file and multi-file OPUS tests now pass without warnings.

The current CRAN package index contains no reverse dependencies for OpenSpecy.
The package now downloads reference libraries only from AWS; its large guarded
integration test verified the pinned version ID, byte count, SHA-256 digest,
load behavior, and representative matching for all seven runtime artifacts.
