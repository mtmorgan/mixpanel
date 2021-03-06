% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hash.R
\name{hash}
\alias{hash}
\alias{unhash}
\title{hash or unhash columns of a tibble}
\usage{
hash(values, except = NULL)

unhash(keys)
}
\arguments{
\item{values}{character() of values to be hashed.}

\item{except}{character(1) regular expression of values to be
excluded from hashing (appearing in original form in the
output).}

\item{keys}{character() of keys (produced by \code{hash()}) to be
replaced by their values.}
}
\value{
\code{hash()} returns a character vector the same length as the
input, but with values replaced by their hashed
equivalent. Internally, the package stores the hash as a key to
be used to look up the value when using \code{unhash()}.

\code{unhash()} returns a character vector the same llength as
the input, but with keys replaced by their original
values. Elements of \code{key} not hashed in the current R session
are returned unchanged.
}
\description{
\code{hash()} calculates a simple 6-character hash of each
element of a character vector. This can be used for obfuscating
sensitive information. \code{hash()} is NOT appropriate for
de-identification because (a) the algorithm is not
cryptographically secure and (b) the hash cannot be easily
reversed in a separate R session or without the original
values.

\code{unhash()} takes a vector of keys and returns their
original values. \code{unhash()} can only work in the same R session
as \code{hash()} on the original data.
}
\examples{
month.name
keys <- hash(month.name, except = "^Ma")
keys

unhash(keys)

}
