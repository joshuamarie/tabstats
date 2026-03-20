# Display a formatted table in the console

Display a formatted table in the console

## Usage

``` r
table_default(
  x,
  justify_cols = "center",
  digits = 3,
  digits_by_col = NULL,
  scientific = FALSE,
  na_print = "",
  min_width = NULL,
  border_char = options("tab_default")$tab_default$border_char,
  show_row_names = FALSE,
  center_table = FALSE,
  n_space = 2,
  title = NULL,
  style_colnames = NULL,
  style_columns = NULL,
  nrows = getOption("tab_default")$nrows,
  vb = list(),
  auto_wrap = TRUE,
  wrap_threshold = 1,
  ...
)
```

## Arguments

- x:

  A data frame or tibble.

- justify_cols:

  Alignment: a single string, vector, or named list of
  "left"/"right"/"center".

- digits:

  Digits to round numeric columns to. Default `3`.

- digits_by_col:

  Named list of per-column digit overrides.

- scientific:

  Display numerics in scientific notation? Default `FALSE`.

- na_print:

  String for missing values. Default `""`.

- min_width:

  Minimum column width. Default `NULL`.

- border_char:

  Character for borders. Default `"\u2500"`.

- show_row_names:

  Show row names? Default `FALSE`.

- center_table:

  Center table in terminal? Default `FALSE`.

- n_space:

  Spaces between columns. Default `2`.

- title:

  Optional title string above the table. from
  [`td_style()`](https://joshuamarie.github.io/tabstats/reference/td_style.md),
  or a named list where each name is a column name or `"title"`, and
  each value is either a cli style string (e.g. `"blue_bold"`) or a
  function `\(ctx) ...` receiving a context list.

- style_colnames:

  Styling for column header cells. A `td_style` object from
  [`td_style()`](https://joshuamarie.github.io/tabstats/reference/td_style.md),
  or a named list where each name is a column name or `"title"`, and
  each value is either a cli style string (e.g. `"blue_bold"`) or a
  function `\(ctx) ...` receiving a context list.

- style_columns:

  Styling for data cells. A `td_style` object from
  [`td_style()`](https://joshuamarie.github.io/tabstats/reference/td_style.md),
  or a named list where each name is a column name or column index as a
  string, and each value is a cli style string or a function
  `\(ctx) ...` receiving a context list with elements `value`,
  `formatted_value`, `col_name`, `col_index`, `is_header`, `data`,
  `justify`, and `width`.

- nrows:

  Max rows to display before truncation.

- vb:

  Vertical border spec: `list(char = "\u2502", after = c(1, 3))`.

- auto_wrap:

  Auto-wrap wide tables? Default `TRUE`.

- wrap_threshold:

  Fraction of console width before wrapping. Default `1`.

- ...:

  Reserved for future use.

## Value

Invisibly returns the input data as a character matrix after formatting
has been applied. The function is called primarily for its side effect
of printing a styled table to the R console. Returns `invisible(NULL)`
early if the input has 0 rows and 0 columns, or if it has 0 columns.

## Examples

``` r
table_default(head(mtcars))
#> ────────────────────────────────────────────────────────────────────
#>    mpg    cyl  disp  hp   drat    wt     qsec   vs  am  gear  carb  
#> ────────────────────────────────────────────────────────────────────
#>   21.000   6   160   110  3.900  2.620  16.460  0   1    4     4    
#>   21.000   6   160   110  3.900  2.875  17.020  0   1    4     4    
#>   22.800   4   108   93   3.850  2.320  18.610  1   1    4     1    
#>   21.400   6   258   110  3.080  3.215  19.440  1   0    3     1    
#>   18.700   8   360   175  3.150  3.440  17.020  0   0    3     2    
#>   18.100   6   225   105  2.760  3.460  20.220  1   0    3     1    
#> ────────────────────────────────────────────────────────────────────
table_default(head(mtcars), style_columns = td_style(mpg = "cyan", cyl = "magenta"))
#> ────────────────────────────────────────────────────────────────────
#>    mpg    cyl  disp  hp   drat    wt     qsec   vs  am  gear  carb  
#> ────────────────────────────────────────────────────────────────────
#>   21.000   6   160   110  3.900  2.620  16.460  0   1    4     4    
#>   21.000   6   160   110  3.900  2.875  17.020  0   1    4     4    
#>   22.800   4   108   93   3.850  2.320  18.610  1   1    4     1    
#>   21.400   6   258   110  3.080  3.215  19.440  1   0    3     1    
#>   18.700   8   360   175  3.150  3.440  17.020  0   0    3     2    
#>   18.100   6   225   105  2.760  3.460  20.220  1   0    3     1    
#> ────────────────────────────────────────────────────────────────────
```
