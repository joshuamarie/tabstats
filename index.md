# tabstats

[![CRAN
status](https://www.r-pkg.org/badges/version/tabstats)](https://CRAN.R-project.org/package=tabstats)
[![R-CMD-check](https://github.com/joshuamarie/tabstats/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/joshuamarie/tabstats/actions/workflows/R-CMD-check.yaml)

## Package overview

Title: ***A Lightweight Toolkit for Displaying Customizable Tables***

***tabstats*** is a lightweight package in action, ideal for
development, that allows end users to print out the data they want to
present as a table in the command line, with further enhancements that
modify the table with styles and alignment. This is very useful when
displaying statistical results on the R console/command line as a table.

## Installation

Install the following package from CRAN:

``` r
install.packages("tabstats")
```

Or install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("joshuamarie/tabstats")
## devtools::install_github("joshuamarie/tabstats") 
```

## Example

Here’s a basic example that demonstrates how to use `tabstats` to format
and style a table:

``` r
table_default(head(mtcars, 5))
#> ────────────────────────────────────────────────────────────────────
#>    mpg    cyl  disp  hp   drat    wt     qsec   vs  am  gear  carb  
#> ────────────────────────────────────────────────────────────────────
#>   21.000   6   160   110  3.900  2.620  16.460  0   1    4     4    
#>   21.000   6   160   110  3.900  2.875  17.020  0   1    4     4    
#>   22.800   4   108   93   3.850  2.320  18.610  1   1    4     1    
#>   21.400   6   258   110  3.080  3.215  19.440  1   0    3     1    
#>   18.700   8   360   175  3.150  3.440  17.020  0   0    3     2    
#> ────────────────────────────────────────────────────────────────────
```

This package allows end users to apply advanced styling and alignment to
tables, such as color coding, column alignment, and borders.

## License

MIT + file LICENSE

## Code of Conduct

Please note that the `tabstats` project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
