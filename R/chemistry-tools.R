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



#' Vapour pressure of water
#'
#' Vapour pressure of water as a function of temperature
#' This function returns the vapour pressure of water at the given
#' temperature(s) from the common::vapourwater dataset.
#'
#' @param temperature numeric vector, in degrees Celsius
#'
#' @return vapour pressure of water, in kilopascal
#' @export
#'
#' @examples
#' \dontrun{
#' VapourPressureWater(45)
#' VapourPressureWater(c(20, 25, 45, 60))
#' }
VapourPressureWater <- function(temperature) {
   data <- common::vapourwater
   # if T outside range in data, warn the user
   if (any(temperature < min(data$temperature)) | any(temperature > max(data$temperature))) {
      warning("At least one supplied temperature is outside the data range (",
           paste(range(data$temperature), collapse=" - "),
           " Celsius). Returning NAs for those values.")
   }
   # The vapourwater dataset only contains data for every degree celsius (or less),
   # so we use the interpolation function to fill in the rest.
   # Note that approx(rule = 1) returns NAs for input outside the data range.
   pressure <-
      stats::approx(x = data$temperature, y = data$pressure,
                    method = "linear", rule = 1,
                    xout = temperature)$y
   return(pressure)
}



#' Oxygen solubility in water
#'
#' Oxygen solubility in water  which is in contact with
#' air saturated with water vapour, as a function of
#' temperature and at a total pressure of 760 torr.
#'
#' Some background: as the temperature of a gasesous solution is raised the
#' gas is driven off until complete degassing occurs at the boiling point
#' of the solvent. This variation of solubility with temperature can be
#' derived from thermodynamic first principles.
#' But the variation of oxygen solubility in water cannot be represented by a
#' simple relationship (derived from thermodynamic first principles), and so
#' more complicated expressions which are fitted to empirical data have
#' to be used.
#'
#' Hitchman, Measurement of Dissolved Oxygen, 1978 reproduce a table by
#' Battino and Clever (1966) that presents experimental values of the
#' so-called Bunsen absorption coefficient (this is the volume of gas, at 0 C
#' and 760 torr, that, at the temperature of measurement, is dissolved in one
#' volume of the solvent when the partial pressure of the gas is 760 torr)
#' recorded by eleven research groups up until 1965. The standard error of the
#'  mean value is never greater +-0.5%. The mean values from this table are
#' probably accurate enough for most applications.
#' Hitchman notes that the data in this table can be fitted by two forms of
#' equations: one form obtained from Henry's law (under the restriction that
#' the partial pressure of the gas remains constant), and another form by
#' describing the variation with temperature by fitting a general power series.
#' The latter approach is used in this function.
#'
#' Hitchman chooses to fit a fourth degree polynomial, and found that the
#' square of the correlation coefficient was 0.999996.
#'
#' For more background and detailed derivation of the formula used here,
#' see section 2.2 (pp. 11) in Hitchman.
#'
#' This formula is strictly speaking only valid for 0 < T < 50 celsius.
#' The function will return values outside this range, but with a warning.
#'
#' @param temperature numeric, vector. In degrees Celsius.
#'
#' @return a dataframe with the following columns:
#'     + "temperature" same as the supplied temperature
#'     + "g/cm-3" oxygen solubility expressed as gram per cubic cm
#'     + "mg/L" ditto expressed as milligram per litre
#'     + "mol/L" ditto expressed as moles per litre (molarity)
#'     + "permoleculewater" number of O2 molecules per molecule of water
#'    Note: mg/L is equivalent to ppm by weight (since water has approx
#'    unit density in the temperature range 0-50 Celsius).
#' @export
#' @examples
#' \dontrun{
#' OxygenSolubilityWater(22)
#' OxygenSolubilityWater(c(2, 7, 12, 30))
#' }
OxygenSolubilityWater <- function(temperature) {
   if (any(temperature < 0) | any(temperature > 50)) {
      warning("This function is fitted to data within the range 0 - 50 Celsius. ",
              "You are now extrapolating.")
   }
   # formula weight of oxygen
   oxygen.fw <- 15.9994 # gram per mole
   # formula weight of water
   water.fw <- 18.02
   # oxygen ratio in dry air (20.95%)
   oxygen.dryair <- 20.95 / 100
   # coefficients (from Hitchman)
   A <-  4.9E1
   B <- -1.335
   C <-  2.759E-2
   D <- -3.235E-4
   E <-  1.614E-6
   # conversion factor from Bunsen to g/cm-3
   conv.factor <- (2 * oxygen.fw) / 22.414E3
   # Bunsen absorption coefficient, commonly denoted as Greek alpha
   alpha <-
      1E-3 * (A + B * temperature + C * temperature^2 +
                 D * temperature^3 + E * temperature^4)
   # solubility of oxygen, in gram per cm-3
   oxygen.solubility <-
      data.frame(temperature = temperature,
         grampercm3 =
           (conv.factor * oxygen.dryair / 760) * alpha *
           # keep in mind that VapourPressureWater() returns values in kilopascal
           (760 - pascal2torr(1E3 * common::VapourPressureWater(temperature))),
        mgperlitre =
           1E6 * (conv.factor * oxygen.dryair / 760) * alpha *
           (760 - pascal2torr(1E3 * common::VapourPressureWater(temperature))),
        molperlitre =
           1E3 * (conv.factor * oxygen.dryair / 760) * alpha *
           (760 - pascal2torr(1E3 * common::VapourPressureWater(temperature))) /
           (2 * oxygen.fw))
   # Number of O2 molecules per water molecule
   oxygen.solubility$permoleculewater <-
      # note: using a water density value that's approx the average in the
      # temperature range 0 - 50 Celsius
      ((1E3 * oxygen.solubility$grampercm3) / oxygen.fw) / (995.00 / water.fw)
   return(oxygen.solubility)
}
