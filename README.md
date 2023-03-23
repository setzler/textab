textab: Create Highly-Customized LaTeX tables in R
================

Created by Thibaut Lamadon and Bradley Setzler

The `textab` package produces highly-customized LaTeX tables in R.

There are only three functions in this package:

- `TexRow`: Form a row in a LaTeX tabular environment.
- `TexSave`: Save the tabular as a .tex file, and optionally compile it
  as a PDF.
- `TexMidrule`: Insert a midrule, or list of partial midrules, between
  rows in a LaTeX tabular environment.

This package builds LaTeX tables in blocks, in the spirit of `ggplot2`,
using the `+` and `-` operators for concatenation.

- minus sign (`-`): Combine rows side-by-side (forming a wider row).
- plus sign (`+`): Stack rows vertically (for multiple rows).

To install the package:

``` r
devtools::install_github("setzler/textab")
```

To use the package after it is installed:

``` r
library(textab)
```
