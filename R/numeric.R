#' Convert arcminutes to degrees
#'
#' Convert arcminutes to degrees
#'
#' @param x arcminutes, numeric
#'
#' @seealso https://en.wikipedia.org/w/index.php?title=Minute_and_second_of_arc&oldid=1159601591
#'
#' @return degrees, numeric
#' @export
amin2degrees <- function(x) {
   return (x / 60)
}


#' Convert arcseconds to degrees
#'
#' Convert arcseconds to degrees
#'
#' @param x arcseconds, numeric
#'
#' @seealso https://en.wikipedia.org/w/index.php?title=Minute_and_second_of_arc&oldid=1159601591
#'
#' @return degrees, numeric
#' @export
asec2degrees <- function(x) {
   return (common::amin2degrees(x / 60))
}


#' Convert degrees to arcminutes
#'
#' Convert degrees to arcminutes
#'
#' @param x degrees, numeric
#'
#' @return arcminutes, numeric
#' @export
degrees2amin <- function(x) {
   return (60 * x)
}


#' Convert degrees to arcseconds
#'
#' Convert degrees to arcseconds
#'
#' @param x degrees, numeric
#'
#' @return arcseconds, numeric
#' @export
degrees2asec <- function(x) {
   return (60 * common::degrees2amin(x))
}


#' Calculate area under a curve
#'
#' Numerically calculate area under an arbitrary curve (defined by x, y coord pairs)
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


#' Format numbers using SI unit prefixes
#'
#' Using SI unit prefixes is a more compact way to write very large or very small
#' numbers which can sometimes be useful. This function takes a number (or a vector
#' of numbers) and returns the equivalent value expressed using the nearest prefix.
#' My thanks to https://stackoverflow.com/users/843265/tomelgin for posting the
#' code that this function was based on (see first link below).
#'
#' @seealso https://stackoverflow.com/questions/11340444/convert-numbers-to-si-prefix
#' @seealso https://stackoverflow.com/questions/28159936/format-numbers-with-million-m-and-billion-b-suffixes
#' @seealso https://www.nist.gov/pml/owm/metric-si-prefixes
#'
#' @param number   number, numeric vector
#' @param rounding rounds number to nearest integer (default FALSE), boolean
#' @param digits   if rounding=FALSE, lets you specify significant figures (default 6), numeric
#'
#' @return number followed by SI prefix (as character string,
#'     separated by narrow no-break space)
#' @export
numbers2prefix <- function(number, rounding = FALSE, digits = ifelse(rounding, NA, 6)) {
   # https://www.nist.gov/pml/owm/metric-si-prefixes
   # https://en.wikipedia.org/wiki/Unit_prefix
   # https://blog.ansi.org/anab/new-prefixes-si-ronto-quecto-ronna-quetta
   lut <- tibble::tribble(
      ~factor, ~symbol,
      1e-30,   "q",      # quecto
      1e-27,   "r",      # ronto
      1e-24,   "y",      # yocto
      1e-21,   "z",      # zepto
      1e-18,   "a",      # atto
      1e-15,   "f",      # femto
      1e-12,   "p",      # pico
      1e-09,   "n",      # nano
      1e-06,   "\u00B5", # micro
      1e-03,   "m",      # milli
      1e-02,   "c",      # centi
      1e-01,   "d",      # deci
      1,       "",
      1e03,    "k",      # kilo
      1e06,    "M",      # mega
      1e09,    "G",      # giga
      1e12,    "T",      # tera
      1e15,    "P",      # peta
      1e18,    "E",      # exa
      1e21,    "Z",      # zetta
      1e24,    "Y",      # yotta
      1e27,    "R",      # ronna
      1e30,    "Q")      # quetta

   # note that findInterval() requires vec to be sorted non-decreasingly and not contain NAs
   ix <- findInterval(x = number, vec = lut$factor)
   if (ix > 0 && ix < length(lut$factor) && lut$factor[ix] != 1) {
      if (rounding == TRUE && !is.numeric(digits)) {
         sistring <- paste0(round(number / lut$factor[ix]), "\u202f", lut$symbol[ix])
      } else
         if (rounding == TRUE || is.numeric(digits)) {
            sistring <- paste0(signif(number / lut$factor[ix], digits), "\u202f", lut$symbol[ix])
         } else {
            sistring <- paste0(number / lut$factor[ix], "\u202f", lut$symbol[ix])
         }
   } else {
      sistring <- as.character(number)
   }
   return(sistring)
}
