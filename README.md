textab: Create Highly-Customized LaTeX Tables in R
================

[![](http://cranlogs.r-pkg.org/badges/grand-total/textab?color=blue)](https://cran.r-project.org/package=textab)
[![](https://www.r-pkg.org/badges/version/textab?color=blue)](https://cran.r-project.org/package=textab)
[![CRAN
checks](https://badges.cranchecks.info/summary/textab.svg)](https://cran.r-project.org/web/checks/check_results_textab.html)
[![](https://img.shields.io/badge/devel%20version-1.0.0.9000-blue.svg)](https://github.com/setzler/textab)
[![](https://img.shields.io/github/last-commit/setzler/textab.svg)](https://github.com/setzler/textab/commits/main)

Created by Thibaut Lamadon and Bradley Setzler

The `textab` package produces highly-customized LaTeX tables in R.

There are only three functions in this package:

- `TexRow`: Form a row in a LaTeX tabular environment.
- `TexSave`: Save the tabular as a .tex file, and optionally compile it
  as a PDF.
- `TexMidrule`: Insert a midrule, or list of partial midrules, between
  rows in a LaTeX tabular environment.

This package builds LaTeX tables in blocks, in the spirit of `ggplot2`,
using the `+` and `/` operators for concatenation.

- plus sign (`+`): Stack rows vertically (forming multiple rows).
- slash sign (`/`): Combine rows side-by-side (forming a wider row).

To install the package from CRAN:

``` r
install.packages("textab")
```

To install the package from Github:

``` r
devtools::install_github("setzler/textab")
```

To use the package after it is installed:

``` r
library(textab)
```

To get started, read the following articles:

- [Get Started](https://setzler.github.io/textab/articles/textab.html)
- [Function
  Documentation](https://setzler.github.io/textab/reference/index.html)
- [Formatting
  Numbers](https://setzler.github.io/textab/articles/Numerics.html)
