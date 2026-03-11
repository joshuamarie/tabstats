# Generate and Display a Cross Tabulation Table

Generate and Display a Cross Tabulation Table

## Usage

``` r
cross_table(
  data,
  percentage = NULL,
  layout = TRUE,
  expected = TRUE,
  layout_center = FALSE,
  header_underline = FALSE,
  digits = NULL,
  center_table = FALSE,
  style = NULL,
  ...
)
```

## Arguments

- data:

  A matrix or table of observed frequencies.

- percentage:

  Which percentages to show. Options: `TRUE`/`"all"`, `"by_row"`,
  `"by_col"`, `"by_total"`, `FALSE`/`NULL`.

- layout:

  Show layout legend box? Default `TRUE`.

- expected:

  Show expected frequencies? Default `TRUE`.

- layout_center:

  Center the layout box? Default `FALSE`.

- header_underline:

  Shorten the underline beneath the column header? Default `FALSE`.

- digits:

  Named list with keys `ex`, `row_pct`, `col_pct`, `total_pct`. Falls
  back to `getOption("ct_digits")`.

- center_table:

  Center entire table in terminal? Default `FALSE`.

- style:

  Named list supplied using
  [`ct_style()`](https://joshuamarie.github.io/tabstats/reference/ct_style.md).

- ...:

  Reserved for future use.

## Value

Invisibly returns the formatted cross-tabulation matrix.

## Examples

``` r
cross_table(matrix(c(10, 20, 30, 40), nrow = 2))
#> 
#> ┌──────────────────────────────────┐ 
#> |      Layout for Cont. Table      | 
#> ├──────────────────────────────────┤ 
#> |  < Freq > (< Expected Value >)   | 
#> └──────────────────────────────────┘ 
#> 
#>       Cross Tabulation: x by y       
#> ─────────────────────────────────────
#>                  y            
#> ─────────────────────────────────────
#>   x        Col1      Col2     TOTAL  
#> ─────────────────────────────────────
#>   Row1    10 (12)   30 (28)    40    
#> 
#>   Row2    20 (18)   40 (42)    60    
#> ─────────────────────────────────────
#>   TOTAL     30        70       100   
#> ─────────────────────────────────────
cross_table(matrix(c(10, 20, 30, 40), nrow = 2), percentage = "all")
#> 
#> ┌──────────────────────────────────┐ 
#> |      Layout for Cont. Table      | 
#> ├──────────────────────────────────┤ 
#> |  < Freq > (< Expected Value >)   | 
#> |        < % by total row >        | 
#> |      < % by total column >       | 
#> |       < % by grand total >       | 
#> └──────────────────────────────────┘ 
#> 
#>       Cross Tabulation: x by y       
#> ─────────────────────────────────────
#>                  y            
#> ─────────────────────────────────────
#>   x        Col1      Col2     TOTAL  
#> ─────────────────────────────────────
#>   Row1    10 (12)   30 (28)    40    
#>             25%       75%      40%   
#>             33%       43%            
#>             10%       30%            
#> 
#>   Row2    20 (18)   40 (42)    60    
#>             33%       67%      60%   
#>             67%       57%            
#>             20%       40%            
#> ─────────────────────────────────────
#>   TOTAL     30        70       100   
#>             30%       70%            
#> ─────────────────────────────────────
```
