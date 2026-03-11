# Getting Started

## Rationale

[tabstats](https://github.com/joshuamarie/tabstats) is a lightweight
package for displaying data in clean, formatted tables directly in the
console. It is mainly designed for developers who wants to display their
outputs (preferably in a data frame object), and to make the output
readable at a glance, configurable without boilerplate, and friendly to
interactive workflows.

The package provides four main functions:

| Function                                                                               | Purpose                                    |
|----------------------------------------------------------------------------------------|--------------------------------------------|
| [`table_summary()`](https://joshuamarie.github.io/tabstats/reference/table_summary.md) | Two-column key-value summary table         |
| [`table_default()`](https://joshuamarie.github.io/tabstats/reference/table_default.md) | General-purpose data frame table           |
| [`corr_matrix()`](https://joshuamarie.github.io/tabstats/reference/corr_matrix.md)     | Correlation matrix display                 |
| [`cross_table()`](https://joshuamarie.github.io/tabstats/reference/cross_table.md)     | Cross tabulation with optional percentages |

All functions share a common design philosophy:

1.  **Styling** is controlled via a `style = list(...)` argument that
    accepts either predefined style strings (e.g. `"blue_bold"`) or
    plain lambda functions (e.g. `\(x) cli::col_red(x)`).
2.  **Alignment** is controlled via an `align` argument that accepts a
    single string, a length-2 vector, or a named list.
3.  **Centering** is controlled via `center_table = TRUE`, which
    automatically pads the output to the middle of your terminal.

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

## A Quick Tour

The main API of this package displays the data you wanted to display
into a specific format

### `table_default()`

Data frames usually looks like this when displayed:

``` r
head(mtcars[, 1:5], 5)
```

``` r-output
                   mpg cyl disp  hp drat
Mazda RX4         21.0   6  160 110 3.90
Mazda RX4 Wag     21.0   6  160 110 3.90
Datsun 710        22.8   4  108  93 3.85
Hornet 4 Drive    21.4   6  258 110 3.08
Hornet Sportabout 18.7   8  360 175 3.15
```

With
[`table_default()`](https://joshuamarie.github.io/tabstats/reference/table_default.md),
the data frame is now displayed in an APA-style format, with
configurable formatting.

``` r
table_default(head(mtcars[, 1:5], 5))
```

``` r-output
─────────────────────────────────
   mpg    cyl  disp  hp   drat   
─────────────────────────────────
  21.000   6   160   110  3.900  
  21.000   6   160   110  3.900  
  22.800   4   108   93   3.850  
  21.400   6   258   110  3.080  
  18.700   8   360   175  3.150  
─────────────────────────────────
```

### `table_summary()`

Another form of
[`table_default()`](https://joshuamarie.github.io/tabstats/reference/table_default.md),
except it is best suitable for displaying summarized data, where the
data frame has 2 columns. The simplest use case is a named summary of
values, like model diagnostics or descriptive statistics.

``` r
df = data.frame(
    Statistic = c("N", "Mean", "SD", "Min", "Max"),
    Value = c("100", "3.45", "1.20", "1.00", "6.00")
)

table_summary(
    df, 
    title = "Descriptive Statistics", 
    header = TRUE
)
```

``` r-output
Descriptive Statistics
----------------------
  Statistic    Value
----------------------
  N              100
  Mean          3.45
  SD            1.20
  Min           1.00
  Max           6.00
----------------------
```

*The `header` means you still wanted to display the column names from
the original `df` data frame.*

### `corr_matrix()` — correlation matrices

A much complex function to display the matrix, but specifically for
correlation matrices. If the `display` is a correlation matrix,
typically the output of [`cor()`](https://rdrr.io/r/stats/cor.html), you
can directly pass it.

``` r
corr_matrix(cor(mtcars[, 1:4]), method = "Pearson")
```

``` r-output
               Correlation Matrix               
────────────────────────────────────────────────
  Variable    mpg      cyl      disp      hp    
────────────────────────────────────────────────
  mpg          1      -0.852   -0.848   -0.776  
────────────────────────────────────────────────
  cyl        -0.852     1       0.902    0.832  
────────────────────────────────────────────────
  disp       -0.848    0.902     1       0.791  
────────────────────────────────────────────────
  hp         -0.776    0.832    0.791     1     
────────────────────────────────────────────────
```

But, if the data you wanted to display is not a matrix, but on another
form, you gotta have to configure it by building a custom spec with
[`new_corr_spec()`](https://joshuamarie.github.io/tabstats/reference/new_corr_spec.md)
for full control over which values appear.

A wild example, assuming you want to display the output from
[`rstatix::cor_test()`](https://rpkgs.datanovia.com/rstatix/reference/cor_test.html):

``` r
cor_mat = 
    iris |> 
    rstatix::cor_test(Sepal.Width, Sepal.Length, Petal.Length) |> 
    dplyr::mutate(
        var1,
        var2,
        cor = format(cor, digits = 2),
        statistic = format(statistic, digits = 2),
        conf_int = paste0(
            "[",
            format(conf.low, digits = 2), 
            ", ",
            format(conf.high, digits = 2),
            "]"
        ),
        
        .keep = "unused"
    ) 

cor_mat |> 
    with({
        corr_matrix(
            new_corr_spec(
                var1 = var1,
                var2 = var2,
                corr = cor,
                statistic = statistic,
                pval = p,
                conf_int = conf_int
            ),
            title = "Pearson Correlation Matrix"
        )
    })
```

``` r-output
                      Pearson Correlation Matrix                      
──────────────────────────────────────────────────────────────────────
  Variable         Sepal.Width      Sepal.Length      Petal.Length    
──────────────────────────────────────────────────────────────────────
  Sepal.Width           1               -0.12             -0.43       
                                      -1.4e+00          -5.8e+00      
                                        0.152           4.51e-08      
                                   [-0.27,  0.044]   [-0.55, -0.288]  
──────────────────────────────────────────────────────────────────────
  Sepal.Length        -0.12               1                0.87       
                    -1.4e+00                             2.2e+01      
                      0.152                             1.04e-47      
                 [-0.27,  0.044]                     [ 0.83,  0.906]  
──────────────────────────────────────────────────────────────────────
  Petal.Length        -0.43              0.87               1         
                    -5.8e+00           2.2e+01                        
                    4.51e-08          1.04e-47                        
                 [-0.55, -0.288]   [ 0.83,  0.906]                    
──────────────────────────────────────────────────────────────────────
```

### `cross_table()`

Another nice function to display the matrices — contingency tables, to
be exact. It has (almost) everything to display the contingency table,
including the observed frequencies, expected values, and percentages
side by side.

``` r
m = matrix(
    c(10, 20, 30, 40), 
    nrow = 2,
    dimnames = list(
        c("A", "B"), 
        c("X", "Y")
    )
)

cross_table(m, percentage = "all")
```

``` r-output
┌──────────────────────────────────┐ 
|      Layout for Cont. Table      | 
├──────────────────────────────────┤ 
|  < Freq > (< Expected Value >)   | 
|        < % by total row >        | 
|      < % by total column >       | 
|       < % by grand total >       | 
└──────────────────────────────────┘ 

      Cross Tabulation: x by y       
─────────────────────────────────────
                 y            
─────────────────────────────────────
  x          X         Y      TOTAL  
─────────────────────────────────────
  A       10 (12)   30 (28)    40    
            25%       75%      40%   
            33%       43%            
            10%       30%            

  B       20 (18)   40 (42)    60    
            33%       67%      60%   
            67%       57%            
            20%       40%            
─────────────────────────────────────
  TOTAL     30        70       100   
            30%       70%            
─────────────────────────────────────
```

## Styling Primer

All table functions provides an API to style the table.

1.  [`table_default()`](https://joshuamarie.github.io/tabstats/reference/table_default.md)
    has 2 parameters: `style_colnames` to style the column names you
    wanted to design, and `style_columns` if you want to apply the style
    to the entire column you choose to style. Use
    [`td_style()`](https://joshuamarie.github.io/tabstats/reference/td_style.md)
    to configure the style of the column you choose to style.

2.  [`table_summary()`](https://joshuamarie.github.io/tabstats/reference/table_summary.md)
    has a `style` parameter to configure the style of the specific
    column you wanted to design. Use
    [`sm_style()`](https://joshuamarie.github.io/tabstats/reference/sm_style.md)
    to configure the style of the specific column.

3.  [`cross_table()`](https://joshuamarie.github.io/tabstats/reference/cross_table.md)
    has a `style` parameter to configure the style of the data displayed
    by the function. Use
    [`ct_style()`](https://joshuamarie.github.io/tabstats/reference/ct_style.md)
    to configure the style of the displayed values.

4.  [`corr_matrix()`](https://joshuamarie.github.io/tabstats/reference/corr_matrix.md)
    has a `style` parameter to configure the style of the values you
    entered, typically from the `display` argument using
    [`new_corr_spec()`](https://joshuamarie.github.io/tabstats/reference/new_corr_spec.md).
    Use
    [`cm_style()`](https://joshuamarie.github.io/tabstats/reference/cm_style.md)
    to configure those values you assigned into
    [`new_corr_spec()`](https://joshuamarie.github.io/tabstats/reference/new_corr_spec.md)
    into the displayed table.

The quickest way to style output is with a named string:

``` r
table_summary(
    df, 
    title = "Descriptive Statistics", 
    header = TRUE, 
    style = sm_style(
        left_col = "blue_bold",
        right_col = "green",
        title = "bold", 
        sep = ":  "
    )
)
```

``` r-output
Descriptive Statistics 
-----------------------
  Statistic    Value
-----------------------
  N         :     100
  Mean      :    3.45
  SD        :    1.20
  Min       :    1.00
  Max       :    6.00
-----------------------
```

For full control, pass a lambda instead — it receives the text as its
argument:

``` r
table_summary(
    df, 
    title = "Descriptive Statistics", 
    header = TRUE, 
    style = sm_style(
        left_col = \(x) cli::col_red(cli::style_bold(x)),
        right_col = \(x) cli::col_cyan(x), 
        title = "bold", 
        sep = ":  "
    )
)
```

``` r-output
Descriptive Statistics 
-----------------------
  Statistic    Value
-----------------------
  N         :     100
  Mean      :    3.45
  SD        :    1.20
  Min       :    1.00
  Max       :    6.00
-----------------------
```

Supported named style strings:

| String                                   | Effect                              |
|------------------------------------------|-------------------------------------|
| `"bold"`                                 | Bold text                           |
| `"italic"`                               | Italic text                         |
| `"blue"`, `"red"`, `"green"`, `"yellow"` | Foreground colour                   |
| `"blue_bold"`                            | Colour + bold (combinable with `_`) |
| `"red_italic"`                           | Colour + italic                     |

More example: Imagine you want to apply for the p-value of the output
from
[`rstatix::cor_test()`](https://rpkgs.datanovia.com/rstatix/reference/cor_test.html),
an earlier example. In
[`corr_matrix()`](https://joshuamarie.github.io/tabstats/reference/corr_matrix.md),
you can even conditionally format the specified value from
[`new_corr_spec()`](https://joshuamarie.github.io/tabstats/reference/new_corr_spec.md).

``` r
cor_mat |> 
    with({
        corr_matrix(
            new_corr_spec(
                var1 = var1,
                var2 = var2,
                corr = cor,
                statistic = statistic,
                pval = p,
                conf_int = conf_int
            ),
            title = "Pearson Correlation Matrix",
            style = cm_style(
                pval = function(x) {
                    x_num = as.numeric(x)
                    if (is.na(x_num) || x_num > 0.05) {
                        cli::style_italic(x) 
                    } else if (x_num > 0.01) {
                        cli::col_red(x)
                    } else {
                        cli::style_bold("<0.001")
                    }
                }
            )
        )
    })
```

``` r-output
                      Pearson Correlation Matrix                      
──────────────────────────────────────────────────────────────────────
  Variable         Sepal.Width      Sepal.Length      Petal.Length    
──────────────────────────────────────────────────────────────────────
  Sepal.Width           1               -0.12             -0.43       
                                      -1.4e+00          -5.8e+00      
                                        0.152            <0.001       
                                   [-0.27,  0.044]   [-0.55, -0.288]  
──────────────────────────────────────────────────────────────────────
  Sepal.Length        -0.12               1                0.87       
                    -1.4e+00                             2.2e+01      
                      0.152                              <0.001       
                 [-0.27,  0.044]                     [ 0.83,  0.906]  
──────────────────────────────────────────────────────────────────────
  Petal.Length        -0.43              0.87               1         
                    -5.8e+00           2.2e+01                        
                     <0.001            <0.001                         
                 [-0.55, -0.288]   [ 0.83,  0.906]                    
──────────────────────────────────────────────────────────────────────
```
