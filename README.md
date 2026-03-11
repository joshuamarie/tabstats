
# tabstats

<!-- [![CRAN status](https://www.r-pkg.org/badges/version/tabstats)](https://CRAN.R-project.org/package=tabstats) -->

[![R-CMD-check](https://github.com/joshuamarie/tabstats/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/joshuamarie/tabstats/actions/workflows/R-CMD-check.yaml)
<!-- [![Codecov test coverage](https://codecov.io/gh/joshuamarie/tabstats/graph/badge.svg)](https://app.codecov.io/gh/joshuamarie/tabstats) -->

## Package overview

Title: ***Helpers for Displaying Tables***

***tabstats*** is a lightweight package in action that assists you to
print out the data you want to display as a table, with more
enchantments that customizes table with styles and alignment. This is
especially useful in displaying statistical results in R console/command
line as a table.

> The package is currently **under construction** and not yet available
> on CRAN.

## Installation

You can install the development version of `tabstats` from GitHub with:

``` r
# install.packages("pak")
pak::pak("joshuamarie/tabstats")
```

## Example

Here’s a basic example that demonstrates how to use `tabstats` to format
and style a table:

``` r
library(tabstats)

mtcars |> 
    rstatix::cor_test(wt, disp, mpg) |> 
    dplyr::mutate(
        var1,
        var2,
        cor = format(cor, digits = 2),
        statistic = format(statistic, digits = 2), 
        p = format(p, digits = 2),
        
        .keep = "unused"
    ) |> 
    dplyr::filter(var1 != var2) |> 
    with({
        corr_matrix(
            new_corr_spec(
                var1 = var1,
                var2 = var2,
                corr = cor,
                statistic = statistic,
                pval = p
            ),
            method = "Pearson"
        )
    })
#> 
#>          Pearson Correlation Matrix          
#> ─────────────────────────────────────────────
#>   Variable      wt        disp       mpg     
#> ─────────────────────────────────────────────
#>   wt                      0.89      -0.87    
#>                          1.1e+01   -9.6e+00  
#>                          1.2e-11    1.3e-10  
#> ─────────────────────────────────────────────
#>   disp         0.89                 -0.85    
#>               1.1e+01              -8.7e+00  
#>               1.2e-11               9.4e-10  
#> ─────────────────────────────────────────────
#>   mpg         -0.87      -0.85               
#>              -9.6e+00   -8.7e+00             
#>               1.3e-10    9.4e-10             
#> ─────────────────────────────────────────────
```

This package allows end users to apply advanced styling and alignment to
tables, such as color coding, column alignment, and borders, making it
easier to view large datasets directly in the command line.

Here’s an example:

``` r
mtcars |> 
    rstatix::cor_test(wt, disp, mpg) |> 
    dplyr::mutate(
        var1,
        var2,
        cor = format(cor, digits = 2),
        statistic = format(statistic, digits = 2), 
        p = format(p, digits = 2),
        
        .keep = "unused"
    ) |> 
    with({
        tabstats::corr_matrix(
            new_corr_spec(
                var1 = var1,
                var2 = var2,
                corr = cor,
                statistic = statistic,
                pval = p
            ),
            style = list(
                pval = \(x) if (x > 0.05) cli::style_italic(x) else cli::col_red(x)
            ),
            method = "Pearson",
            layout_view = TRUE
        )
    })
#> 
#> ┌───────────────────────────┐
#> |  Layout for Corr. Matrix  |
#> ├───────────────────────────┤
#> |         < corr >          |
#> |       < statistic >       |
#> |         < pval >          |
#> └───────────────────────────┘
#> 
#> 
#>          Pearson Correlation Matrix          
#> ─────────────────────────────────────────────
#>   Variable      wt        disp       mpg     
#> ─────────────────────────────────────────────
#>   wt            1         0.89      -0.87    
#>                          1.1e+01   -9.6e+00  
#>                          1.2e-11    1.3e-10  
#> ─────────────────────────────────────────────
#>   disp         0.89        1        -0.85    
#>               1.1e+01              -8.7e+00  
#>               1.2e-11               9.4e-10  
#> ─────────────────────────────────────────────
#>   mpg         -0.87      -0.85        1      
#>              -9.6e+00   -8.7e+00             
#>               1.3e-10    9.4e-10             
#> ─────────────────────────────────────────────
```

> Now, the displayed values for `pval` is marked with red color.

## License

MIT + file LICENSE

## Code of Conduct

Please note that the `tabstats` project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
