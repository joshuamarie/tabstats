# Style specification for corr_matrix()

Constructs a validated style object for use with
[`corr_matrix()`](https://joshuamarie.github.io/tabstats/reference/corr_matrix.md).

## Usage

``` r
cm_style(...)
```

## Arguments

- ...:

  Named style entries. Names should match the extra field names passed
  to
  [`new_corr_spec()`](https://joshuamarie.github.io/tabstats/reference/new_corr_spec.md)
  (e.g. `rho`, `pval`, `bf`), or the reserved keys `title` and
  `border_text`.

## Value

An object of class `c("cm_style", "tabstats_style")`.

## Examples

``` r
cm_style(rho = "blue_bold", pval = "red", title = "bold")
#> <tabstats_style / cm_style>
#>   rho: "blue_bold"
#>   pval: "red"
#>   title: "bold"
cm_style(rho = \(x) cli::col_cyan(x))
#> <tabstats_style / cm_style>
#>   rho: <function>
```
