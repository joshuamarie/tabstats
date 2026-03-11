# Style specification for cross_table()

Constructs a validated style object for use with
[`cross_table()`](https://joshuamarie.github.io/tabstats/reference/cross_table.md).

## Usage

``` r
ct_style(
  observed = NULL,
  expected = NULL,
  row_percentage = NULL,
  col_percentage = NULL,
  total_percentage = NULL,
  total = NULL,
  title = NULL,
  border_text = NULL
)
```

## Arguments

- observed:

  Style for observed frequency cells.

- expected:

  Style for expected frequency values.

- row_percentage:

  Style for row percentage values.

- col_percentage:

  Style for column percentage values.

- total_percentage:

  Style for grand total percentage values.

- total:

  Style for total row/column cells.

- title:

  Style for the table title.

- border_text:

  Style for the horizontal border lines.

## Value

An object of class `c("ct_style", "tabstats_style")`.

## Examples

``` r
ct_style(observed = "blue", expected = "yellow", title = "bold")
#> <tabstats_style / ct_style>
#>   observed: "blue"
#>   expected: "yellow"
#>   title: "bold"
ct_style(observed = \(ctx) cli::col_green(ctx$formatted_text))
#> <tabstats_style / ct_style>
#>   observed: <function>
```
