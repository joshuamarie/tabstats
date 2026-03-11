# Style specification for table_default()

Constructs a validated style object for use with
[`table_default()`](https://joshuamarie.github.io/tabstats/reference/table_default.md).

## Usage

``` r
td_style(...)
```

## Arguments

- ...:

  Named style entries. Each name must be a column name, a column index
  as a string (e.g. `"1"`), or `"title"`. Each value is a string or a
  function `\(ctx) ...` where `ctx` is a context list.

## Value

An object of class `c("td_style", "tabstats_style")`.

## Examples

``` r
td_style(mpg = "cyan", cyl = "magenta")
#> <tabstats_style / td_style>
#>   mpg: "cyan"
#>   cyl: "magenta"
td_style(mpg = \(ctx) cli::col_red(ctx$value), title = "bold")
#> <tabstats_style / td_style>
#>   mpg: <function>
#>   title: "bold"
```
