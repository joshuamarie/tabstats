#' Build a correlation display specification
#'
#' Constructs a structured spec object consumed by `corr_matrix()`. Always
#' requires `var1` and `var2` — the pair pattern they encode determines which
#' triangle(s) of the matrix are filled:
#'
#' | Pattern          | Fills                         |
#' |------------------|-------------------------------|
#' | `var1 < var2`    | Lower triangle only           |
#' | `var1 <= var2`   | Lower triangle + diagonal     |
#' | `var1 > var2`    | Upper triangle only           |
#' | `var1 >= var2`   | Upper triangle + diagonal     |
#' | `var1 != var2`   | Both triangles, no diagonal   |
#' | `var1 == var2`   | Full matrix (diag forced to 1)|
#'
#' All additional named vectors become display rows inside each cell, rendered
#' in the order they are supplied.
#'
#' @param var1 Character vector of first variable names per pair.
#' @param var2 Character vector of second variable names per pair.
#' @param ... Named character vectors of equal length to `var1`/`var2`.
#'   Each becomes one display row inside the cell (e.g. `rho`, `pval`, `bf`).
#'
#' @return An object of class `corr_spec`.
#'
#' @examples
#' new_corr_spec(
#'     var1 = c("a", "a", "b"),
#'     var2 = c("b", "c", "c"),
#'     rho  = c("0.89", "0.79", "0.66"),
#'     pval = c("<0.001", "<0.001", "<0.001")
#' )
#'
#' @export
new_corr_spec = function(var1, var2, ...) {
    stopifnot(is.character(var1), is.character(var2))

    n_pairs = length(var1)
    if (length(var2) != n_pairs)
        stop("`var1` and `var2` must be the same length.", call. = FALSE)

    extras = lapply(list(...), as.character)
    for (nm in names(extras)) {
        if (length(extras[[nm]]) != n_pairs)
            stop(
                sprintf("`%s` must have the same length as `var1` (%d).", nm, n_pairs),
                call. = FALSE
            )
    }

    pattern = detect_pattern(var1, var2, n_pairs)
    vars = extract_vars(var1, var2)

    structure(
        list(
            var1 = var1,
            var2 = var2,
            extras = extras,
            pattern = pattern,
            vars = vars
        ),
        class = "corr_spec"
    )
}

detect_pattern = function(var1, var2, n_pairs) {
    all_vars = unique(c(var1, var2))
    n = length(all_vars)

    expected = list(
        lt = n * (n - 1L) / 2L,
        lte = n * (n + 1L) / 2L,
        gt = n * (n - 1L) / 2L,
        gte = n * (n + 1L) / 2L,
        neq = n * (n - 1L),
        all = n * n
    )

    # Use position in sorted unique vars to determine triangle direction
    sorted_vars = sort(all_vars)
    idx1 = match(var1, sorted_vars)
    idx2 = match(var2, sorted_vars)
    has_lt = any(idx1 < idx2)
    has_gt = any(idx1 > idx2)
    has_eq = any(var1 == var2)

    if      (!has_lt && !has_gt &&  has_eq && n_pairs == expected$all) "full"
    else if ( has_lt &&  has_gt &&  has_eq && n_pairs == expected$all) "full"
    else if ( has_lt &&  has_gt && !has_eq && n_pairs == expected$neq) "neq"
    else if ( has_lt && !has_gt && !has_eq && n_pairs == expected$lt)  "lt"
    else if ( has_lt && !has_gt &&  has_eq && n_pairs == expected$lte) "lte"
    else if (!has_lt &&  has_gt && !has_eq && n_pairs == expected$gt)  "gt"
    else if (!has_lt &&  has_gt &&  has_eq && n_pairs == expected$gte) "gte"
    else stop(
        sprintf(
            "Vector length (%d) does not match any valid pattern for %d variable(s).\n",
            n_pairs, n
        ),
        "Expected lengths: lt/gt = ", expected$lt,
        ", lte/gte = ", expected$lte,
        ", neq = ", expected$neq,
        ", full = ", expected$all,
        call. = FALSE
    )
}

extract_vars = function(var1, var2) {
    seen = character(0)
    for (v in c(var1, var2))
        if (!v %in% seen) seen = c(seen, v)
    seen
}
