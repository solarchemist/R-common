#' LoadRData2Variable
#'
#' This function loads R-data file into a variable instead of into the workspace.
#' Works well when the R-data file contains only ONE variable.
#' Not tested for when the R-data file contains more than one variable.
#'
#' @param FullPathToRData path to rda file
#'
#' @return an R object, you will probable want to assign it to a variable
#' @export
LoadRData2Variable <- function(FullPathToRData) {
   return(eval(parse(text = load(FullPathToRData))))
}


#' Create a string of specified width from an integer
#'
#' Converts an integer or a vector of integers to a string
#' of fixed length padded with a specified character (e.g., zeroes).
#'
#' @param ii    integer or vector of integers
#' @param pchr  a padding character (e.g., "0")
#' @param w     width of the return string (an integer)
##              Make sure to set the width longer than or equal to the length of the biggest integer.
##              For example, if the integers (ii) are in the range 1 - 100, set w to at least 3.
#'
#' @return a string or a vector of strings
#' @export
int2padstr <- function (ii, pchr, w) {
   gsub(" ", pchr, formatC(ii, format = "s", mode = "character", width = w))
}


#' numbers2words
#'
#' Converts a number into its corresponding words in English
#' THIS FUNCTION WAS PUBLISHED IN: R-News, vol 5, iss 1, May 2005, pp. 51
#' Original author: John Fox, Department of Sociology, McMaster University, Hamilton, Ontari
#'                  Canada L8S 4M4, 905-525-9140x23604
#'                  http://socserv.mcmaster.ca/jfox
#' http://cran.csiro.au/doc/Rnews/Rnews_2005-1.pdf
#' http://finzi.psych.upenn.edu/R/Rhelp02a/archive/46843.html
#'
#' @param x         number
#' @param billion   follow either US or UK usage rules
#' @param and       follows the choice set in billion arg
#'
#' @return string
#' @export
numbers2words <- function(x, billion = c("US", "UK"), and = if (billion == "US") "" else "and") {
   billion <- match.arg(billion)
   trim <- function(text) {
      gsub("(^\ *)|((\ *|-|,\ zero|-zero)$)", "", text)
   }
   makeNumber <- function(x) as.numeric(paste(x, collapse = ""))
   makeDigits <- function(x) strsplit(as.character(x), "")[[1]]
   helper <- function(x) {
      negative <- x < 0
      x <- abs(x)
      digits <- makeDigits(x)
      nDigits <- length(digits)
      result <- if (nDigits == 1) as.vector(ones[digits])
      else if (nDigits == 2)
         if (x <= 19) as.vector(teens[digits[2]])
      else trim(paste(tens[digits[1]], "-", ones[digits[2]], sep=""))
      else if (nDigits == 3) {
         tail <- makeNumber(digits[2:3])
         if (tail == 0) paste(ones[digits[1]], "hundred")
         else trim(paste(ones[digits[1]], trim(paste("hundred", and)),
                         helper(tail)))
      } else {
         nSuffix <- ((nDigits + 2) %/% 3) - 1
         if (nSuffix > length(suffixes) || nDigits > 15)
            stop(paste(x, "is too large!"))
         pick <- 1:(nDigits - 3 * nSuffix)
         trim(paste(helper(makeNumber(digits[pick])), suffixes[nSuffix], helper(makeNumber(digits[-pick]))))
      }
      if (billion == "UK") {
         words <- strsplit(result, " ")[[1]]
         if (length(grep("million,", words)) > 1)
            result <- sub(" million, ", ", ", result)
      }
      if (negative) paste("minus", result) else result
   }
   opts <- options(scipen = 100)
   on.exit(options(opts))
   ones <- c("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
   teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", " seventeen", "eighteen", "nineteen")
   names(ones) <- names(teens) <- 0:9
   tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
   names(tens) <- 2:9
   suffixes <- if (billion == "US") {
      c("thousand,", "million,", "billion,", "trillion,")
   } else {
      c("thousand,", "million,", "thousand million,", "billion,")
   }
   x <- round(x)
   if (length(x) > 1) sapply(x, helper) else helper(x)
}