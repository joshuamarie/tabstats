# Manage package options

This function allows retrieving or modifying the package options across
different categories. If called without arguments, it returns all option
categories and their values. If `category` is provided alone, it returns
all options in that category. If `category` and `option` are provided,
it returns the specific option value. If all three parameters are
provided, it updates the specified option.

## Usage

``` r
tabstats_options(category = NULL, option = NULL, value = NULL)
```

## Arguments

- category:

  A character string specifying the option category (e.g.,
  "tab_default", "tab_digits"). If omitted, returns all option
  categories.

- option:

  A character string specifying the option to retrieve or modify within
  the category. For backward compatibility, you can also use a specific
  option name directly as the `category` parameter.

- value:

  The new value to assign to the specified option. If NULL, the function
  returns the current value.

## Value

If no arguments are provided, returns all option categories and their
values. If only `category` is provided, returns all options in that
category. If `category` and `option` are provided without `value`,
returns the current value of that option. If all parameters are
provided, updates the option and returns the updated option category
list invisibly.

## Examples

``` r
# Get all options across all categories
tabstats_options()
#> $tab_default
#> $tab_default$vb_char
#> [1] "│"
#> 
#> $tab_default$vb_top
#> [1] "┬"
#> 
#> $tab_default$vb_mid
#> [1] "┼"
#> 
#> $tab_default$vb_bottom
#> [1] "┴"
#> 
#> $tab_default$border_char
#> [1] "─"
#> 
#> $tab_default$header_underline
#> [1] "─"
#> 
#> $tab_default$truncate_message
#> [1] TRUE
#> 
#> $tab_default$nrows
#> [1] 10
#> 
#> 
#> $tab_digits
#> $tab_digits$ex
#> [1] 1
#> 
#> $tab_digits$row_pct
#> [1] 0
#> 
#> $tab_digits$col_pct
#> [1] 0
#> 
#> $tab_digits$total_pct
#> [1] 0
#> 
#> 

# Get all options in the "tab_default" category
tabstats_options("tab_default")
#> $vb_char
#> [1] "│"
#> 
#> $vb_top
#> [1] "┬"
#> 
#> $vb_mid
#> [1] "┼"
#> 
#> $vb_bottom
#> [1] "┴"
#> 
#> $border_char
#> [1] "─"
#> 
#> $header_underline
#> [1] "─"
#> 
#> $truncate_message
#> [1] TRUE
#> 
#> $nrows
#> [1] 10
#> 

# Get all options in the "tab_digits" category
tabstats_options("tab_digits")
#> $ex
#> [1] 1
#> 
#> $row_pct
#> [1] 0
#> 
#> $col_pct
#> [1] 0
#> 
#> $total_pct
#> [1] 0
#> 

# Get a specific option
tabstats_options("tab_default", "vb_top")
#> [1] "┬"
tabstats_options("tab_digits", "ex")
#> [1] 1

# Using backward compatibility (system will find the right category)
tabstats_options("vb_top")
#> [1] "┬"
tabstats_options("ex")
#> [1] 1

# Modify an option
tabstats_options("tab_default", "border_char", "+")
tabstats_options("tab_digits", "ex", 2)

# Using backward compatibility for modification
tabstats_options("border_char", "+")
tabstats_options("ex", 2)
```
