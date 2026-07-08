# Package index

## Main API

Main functions used to produce tables

### APA-style table

The default table using the data frame

- [`table_default()`](https://s7-stats.github.io/tabstats/reference/table_default.md)
  : Display a formatted table in the console
- [`td_style()`](https://s7-stats.github.io/tabstats/reference/td_style.md)
  : Style specification for table_default()

### Summary Table

Two-column key-value summary display

- [`table_summary()`](https://s7-stats.github.io/tabstats/reference/table_summary.md)
  : Summarize and Display a Two-Column Data Frame as a Formatted Table
- [`sm_style()`](https://s7-stats.github.io/tabstats/reference/sm_style.md)
  : Style specification for table_summary()

### Contingency Table

Complete contingency table displayer

- [`cross_table()`](https://s7-stats.github.io/tabstats/reference/cross_table.md)
  : Generate and Display a Cross Tabulation Table
- [`ct_style()`](https://s7-stats.github.io/tabstats/reference/ct_style.md)
  : Style specification for cross_table()

### Pairwise Matrix Table

SAS-like pairwise matrix table; use for correlation matrices and beyond

- [`pairwise_matrix()`](https://s7-stats.github.io/tabstats/reference/pairwise_matrix.md)
  [`corr_matrix()`](https://s7-stats.github.io/tabstats/reference/pairwise_matrix.md)
  : Display a Pairwise Matrix Table in the Console
- [`new_pairwise_data()`](https://s7-stats.github.io/tabstats/reference/new_pairwise_data.md)
  [`new_corr_data()`](https://s7-stats.github.io/tabstats/reference/new_pairwise_data.md)
  : Build a pairwise matrix display specification
- [`cm_style()`](https://s7-stats.github.io/tabstats/reference/cm_style.md)
  : Style specification for corr_matrix()

## Package Options

Simple function to tweak options in the package

- [`tabstats_options()`](https://s7-stats.github.io/tabstats/reference/tabstats_options.md)
  : Manage package options
