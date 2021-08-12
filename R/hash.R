#' @rdname hash
#'
#' @name hash
#'
#' @title hash or unhash columns of a tibble
#'
#' @useDynLib mixpanel, .registration=TRUE
#'
#' @importFrom dplyr pull
NULL

.hash_store <- new.env(parent = emptyenv())

.hash_insert <-
    function(values)
{
    vapply(values, function(value) {
        key <- .Call(.hash_impl, value)
        .hash_store[[key]] <- value
        key
    }, character(1))
}

.hash_get <-
    function(keys)
{
    vapply(keys, function(key) {
        value <- .hash_store[[key]]
        if (is.null(value)) # not found
            value <- key
        value
    }, character(1))
}

#' @rdname hash
#'
#' @description `hash()` calculates a simple 6-character hash of each
#'     element of a character vector. This can be used for obfuscating
#'     sensitive information. `hash()` is NOT appropriate for
#'     de-identification because (a) the algorithm is not
#'     cryptographically secure and (b) the hash cannot be easily
#'     reversed in a separate R session or without the original
#'     values.
#'
#' @param values character() of values to be hashed.
#'
#' @param except character(1) regular expression of values to be
#'     excluded from hashing (appearing in original form in the
#'     output).
#'
#' @return `hash()` returns a character vector the same length as the
#'     input, but with values replaced by their hashed
#'     equivalent. Internally, the package stores the hash as a key to
#'     be used to look up the value when using `unhash()`.
#'
#' @examples
#' month.name
#' keys <- hash(month.name, except = "^Ma")
#' keys
#'
#' @export
hash <-
    function(values, except = NULL)
{
    stopifnot(
        `'values' must be a character()` = is.character(values),
        `'except' must be NULL or character(1)` =
            is.null(except) || (is.character(except) && length(except) == 1L)
    )

    index <- !is.na(values)
    if (!is.null(except))
        index <- index & !grepl(except, values)
    values[index] <- .hash_insert(values[index])

    values
}

#' @rdname hash
#'
#' @description `unhash()` takes a vector of keys and returns their
#'     original values. `unhash()` can only work in the same R session
#'     as `hash()` on the original data.
#'
#' @param keys character() of keys (produced by `hash()`) to be
#'     replaced by their values.
#'
#' @return `unhash()` returns a character vector the same llength as
#'     the input, but with keys replaced by their original
#'     values. Elements of `key` not hashed in the current R session
#'     are returned unchanged.
#'
#' @examples
#' unhash(keys)
#'
#' @export
unhash <-
    function(keys)
{
    stopifnot(`'keys' must be character()` = is.character(keys))

    index <- !is.na(keys)
    keys[index] <- .hash_get(keys[index])

    keys
}
