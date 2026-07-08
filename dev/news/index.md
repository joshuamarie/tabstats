# Changelog

## tabstats (development version)

- Fixes
  [`pairwise_matrix()`](https://s7-stats.github.io/tabstats/dev/reference/pairwise_matrix.md)
  discrepancies, where the title from the layout guide box is truncated.

- Documenting hex color codes usage in
  [tabstats](https://github.com/s7-stats/tabstats) under
  [`vignette("tabstats")`](https://s7-stats.github.io/tabstats/dev/articles/tabstats.md).

- [tabstats](https://github.com/s7-stats/tabstats) GitHub repository is
  now under S7 stats. Changing link to
  <https://s7-stats.github.io/tabstats/>.

## tabstats 0.2.0

CRAN release: 2026-05-25

- Added
  [`pairwise_matrix()`](https://s7-stats.github.io/tabstats/dev/reference/pairwise_matrix.md)
  and
  [`new_pairwise_data()`](https://s7-stats.github.io/tabstats/dev/reference/new_pairwise_data.md)
  as generalized equivalents of
  [`corr_matrix()`](https://s7-stats.github.io/tabstats/dev/reference/pairwise_matrix.md)
  and
  [`new_corr_data()`](https://s7-stats.github.io/tabstats/dev/reference/new_pairwise_data.md).
  The new functions work with any pairwise display, not just correlation
  matrices.
- [`corr_matrix()`](https://s7-stats.github.io/tabstats/dev/reference/pairwise_matrix.md)
  and
  [`new_corr_data()`](https://s7-stats.github.io/tabstats/dev/reference/new_pairwise_data.md)
  are retained as aliases for backward compatibility.

## tabstats 0.1.0

CRAN release: 2026-03-24

- Initial CRAN submission.
