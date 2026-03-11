# Style specification for table_summary()

Constructs a validated style object for use with
[`table_summary()`](https://joshuamarie.github.io/tabstats/reference/table_summary.md).

## Usage

``` r
sm_style(
  left_col = NULL,
  right_col = NULL,
  border_text = NULL,
  title = NULL,
  sep = NULL
)
```

## Arguments

- left_col:

  Style for the left column. A string (e.g. `"blue_bold"`) or a function
  `\(x) ...`.

- right_col:

  Style for the right column.

- border_text:

  Style for the horizontal border lines.

- title:

  Style for the title text.

- sep:

  A single character used as the column separator (e.g. `"|"`).

## Value

An object of class `c("sm_style", "tabstats_style")`.

## Examples

``` r
sm_style(left_col = "blue_bold", right_col = "green", title = "bold")
#> <tabstats_style / sm_style>
#>   left_col: "blue_bold"
#>   right_col: "green"
#>   title: "bold"
sm_style(left_col = \(x) cli::col_red(x), sep = "|")
#> <tabstats_style / sm_style>
#>   left_col: <function>
#>   sep: "|"
```
