#' Calculate required mass of substance to dissolve
#'
#' You want to prepare a solution of known molarity and volume of
#' a particular substance.
#' This function calculates the required mass to weigh up.
#'
#' @param formulamass of the substance (in grams per mole)
#' @param volume of the final solution (in liters)
#' @param molarity (in moles per liter)
#'
#' @return mass of substance (in grams)
#' @export
molarity2mass <- function(formulamass, volume, molarity) {
   mass <- formulamass * volume * molarity
   # Double-check units:
   # [g * mol-1] * [liter] * [mole * liter-1] = [g]
   return(mass)
}
