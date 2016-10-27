#' Calculate area under a curve
#'
#' Numerically  calculate area under an arbitrary curve (defined by x, y coord pairs)
#' using trapezodial integration. See Wikipedia for more info on trapz integration.
#'
#' @param x  vector (of length n)
#' @param y  vector (of length n)
#'
#' @return vector (of length n - 1)
#' @export
trapz <- function(x, y) {
   idx <- 2:length(x)
   return (as.double((x[idx] - x[idx - 1]) * (y[idx] + y[idx - 1])) / 2)
}

#' Round up to the nearest specified interval
#'
#' This function rounds UP to the nearest interval specified by "nearest"
#' http://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x
#'
#' @param x       number
#' @param nearest nearest interval, e.g., 5, 10, 100, 1000, etc.
#'
#' @return a number
#' @export
roundup <- function(x, nearest=1000) {
   ceiling(max(x+10^-9)/nearest + 1/nearest)*nearest
}


#' is.wholenumber
#'
#' I am not even sure this function is useful any longer.
#' Kept for legacy purposes just in case some old code depends on it.
#' This function was copied from R's documentation (see ?is.integer).
#'
#' @param x   number
#' @param tol machine's double precision
#'
#' @return logical (TRUE or FALSE)
#' @export
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
   abs(x - round(x)) < tol
}
