# Build a correlation display specification

Constructs a structured spec object consumed by
[`corr_matrix()`](https://joshuamarie.github.io/tabstats/reference/corr_matrix.md).
Always requires `var1` and `var2` — the pair pattern they encode
determines which triangle(s) of the matrix are filled:

## Usage

``` r
new_corr_spec(var1, var2, ...)
```

## Arguments

- var1:

  Character vector of first variable names per pair.

- var2:

  Character vector of second variable names per pair.

- ...:

  Named character vectors of equal length to `var1`/`var2`. Each becomes
  one display row inside the cell (e.g. `rho`, `pval`, `bf`).

## Value

An object of class `corr_spec`.

## Details

|                |                                |
|----------------|--------------------------------|
| Pattern        | Fills                          |
| `var1 < var2`  | Lower triangle only            |
| `var1 <= var2` | Lower triangle + diagonal      |
| `var1 > var2`  | Upper triangle only            |
| `var1 >= var2` | Upper triangle + diagonal      |
| `var1 != var2` | Both triangles, no diagonal    |
| `var1 == var2` | Full matrix (diag forced to 1) |

All additional named vectors become display rows inside each cell,
rendered in the order they are supplied.

## Examples

``` r
new_corr_spec(
    var1 = c("a", "a", "b"),
    var2 = c("b", "c", "c"),
    rho  = c("0.89", "0.79", "0.66"),
    pval = c("<0.001", "<0.001", "<0.001")
)
#> $var1
#> [1] "a" "a" "b"
#> 
#> $var2
#> [1] "b" "c" "c"
#> 
#> $extras
#> $extras$rho
#> [1] "0.89" "0.79" "0.66"
#> 
#> $extras$pval
#> [1] "<0.001" "<0.001" "<0.001"
#> 
#> 
#> $pattern
#> [1] "lt"
#> 
#> $vars
#> [1] "a" "b" "c"
#> 
#> attr(,"class")
#> [1] "corr_spec"
```
