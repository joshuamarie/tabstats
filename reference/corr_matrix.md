# Display a Correlation Matrix Table in the Console

Display a Correlation Matrix Table in the Console

## Usage

``` r
corr_matrix(
  display,
  title = NULL,
  diag_1 = TRUE,
  digits = 3,
  layout_view = FALSE,
  layout_center = FALSE,
  center_table = FALSE,
  border_char = getOption("tab_default")$border_char,
  style = list(),
  ...
)
```

## Arguments

- display:

  A `corr_spec` object from
  [`new_corr_spec()`](https://joshuamarie.github.io/tabstats/reference/new_corr_spec.md),
  or a plain symmetric matrix (e.g. from
  [`cor()`](https://rdrr.io/r/stats/cor.html)).

- title:

  Label shown in the title (e.g. `"Pearson Correlation Matrix"`).
  Auto-detected from a `title` attribute on the spec if present.

- diag_1:

  If `TRUE`, diagonal cells always show `"1"`. Default `TRUE`.

- digits:

  Decimal places for numeric formatting. Default `3`.

- layout_view:

  Show a layout legend box above the table? Default `FALSE`.

- layout_center:

  Center the layout box in the terminal? Default `FALSE`.

- center_table:

  Center table in terminal? Default `FALSE`.

- border_char:

  Border character. Default from `getOption("tab_default")`.

- style:

  A
  [`cm_style()`](https://joshuamarie.github.io/tabstats/reference/cm_style.md)
  object. Keys match the extra field names passed to
  [`new_corr_spec()`](https://joshuamarie.github.io/tabstats/reference/new_corr_spec.md)
  (e.g. `rho`, `pval`, `bf`), plus `title` and `border_text`.

- ...:

  Reserved for future use.

## Value

Invisibly returns the rendered character matrix.

## Examples

``` r
# From a plain correlation matrix, e.g. using `cor()`
corr_matrix(cor(mtcars[, 1:4]), title = "Pearson Correlation Matrix")
#> 
#>            Pearson Correlation Matrix           
#> ────────────────────────────────────────────────
#>   Variable    mpg      cyl      disp      hp    
#> ────────────────────────────────────────────────
#>   mpg          1      -0.852   -0.848   -0.776  
#> ────────────────────────────────────────────────
#>   cyl        -0.852     1       0.902    0.832  
#> ────────────────────────────────────────────────
#>   disp       -0.848    0.902     1       0.791  
#> ────────────────────────────────────────────────
#>   hp         -0.776    0.832    0.791     1     
#> ────────────────────────────────────────────────

# Customizable example
spec = new_corr_spec(
    var1 = c("a", "a", "b"),
    var2 = c("b", "c", "c"),
    rho = c("0.89", "0.79", "0.66"),
    pval = c("<0.001", "<0.001", "<0.001")
)
corr_matrix(spec, title = "Pearson Correlation Matrix", layout_view = TRUE)
#> 
#> ┌───────────────────────────┐
#> |  Layout for Corr. Matrix  |
#> ├───────────────────────────┤
#> |          < rho >          |
#> |         < pval >          |
#> └───────────────────────────┘
#> 
#> 
#>     Pearson Correlation Matrix    
#> ──────────────────────────────────
#>   Variable     a        b      c  
#> ──────────────────────────────────
#>   a                               
#>                                   
#> ──────────────────────────────────
#>   b           0.89                
#>              <0.001               
#> ──────────────────────────────────
#>   c           0.79     0.66       
#>              <0.001   <0.001      
#> ──────────────────────────────────
```
