# nm2eV <- function(nm) {
#    ## Depends on:
#    ## chepec/chetex/common/R/sunlight/solarconstants.R
#    # Converts wavelength in nm to energy in eV
#    #
#    sun.constants <- solar.constants()
#
#    eV <-
#       sun.constants["h.eV", "value"] *
#       1E9 * sun.constants["c", "value"] / nm
#
#    return(eV)
# }


# eV2nm <- function(eV) {
#    ## Depends on:
#    ## chepec/chetex/common/R/sunlight/solarconstants.R
#    # Converts energy in eV to wavelength in nm
#    #
#    sun.constants <- solar.constants()
#
#    nm <-
#       sun.constants["h.eV", "value"] *
#       1E9 * sun.constants["c", "value"] / eV
#
#    return(nm)
# }


#' Convert wavelength to wavenumber
#'
#' Converts wavelength (nm) to wavenumber (cm-1)
#' Only valid for absolute wavelengths, NOT delta wavelengths (ranges)
#' http://www.powerstream.com/inverse-cm.htm
#'
#' @param wavelength number or vector of numbers
#'
#' @return number or vector
#' @export
wavelength2num <- function(wavelength) {
   wavenumber <-
      10E6 / wavelength
   return(wavenumber)
}


#' Convert wavenumber to wavelength
#'
#' Converts wavenumber (cm-1) to wavelength (nm)
#' Only valid for absolute wavenumbers, NOT delta wavenumbers (ranges)
#' http://www.powerstream.com/inverse-cm.htm
#'
#' @param wavenumber  number or vector of numbers
#'
#' @return number ofr vector
#' @export
wavenum2length <- function(wavenumber) {
   wavelength <-
      10E6 / wavenumber
   return(wavelength)
}


#' Convert from radians to degrees
#'
#' @param radians numeric
#'
#' @return degrees (numeric)
#' @export
as.degrees <- function(radians) {
   degrees <- radians * (180 / pi)
   return(degrees)
}


#' Convert from degrees to radians
#'
#' @param degrees numeric
#'
#' @return radians (numeric)
#' @export
as.radians <- function(degrees) {
   radians <- degrees * (pi / 180)
   return(radians)
}


#' Convert from Celsius scale to Kelvin
#'
#' Converts temperature from Celsius to Kelvin.
#'
#' @param Celsius degrees Celsius (numeric)
#'
#' @return Kelvin (numeric)
#' @export
Celsius2Kelvin <- function(Celsius) {
   # Check and correct for values below -273.15
   if (Celsius < -273.15) {
      # If Celsis is less than absolute zero, set it to absolute zero
      Celsius <- -273.15
   }
   Kelvin <- Celsius + 273.15
   return(Kelvin)
}


#' Convert from Kelvin to Celsius scale
#'
#' Converts from temperature in Kelvin to degrees Celsius
#'
#' @param Kelvin (numeric)
#'
#' @return degrees Celsius (numeric)
#' @export
Kelvin2Celsius <- function(Kelvin) {
   # Check and correct for negative values
   if (Kelvin < 0) {
      # If Kelvin is less than zero, set it to zero
      Kelvin <- 0
   }
   Celsius <- Kelvin - 273.15
   return(Celsius)
}


#' Calculate d-spacings from 2theta values
#'
#' This function applies Bragg's law to calculate d-spacings from thth (n = 1)
#'
#' @param thth        vector with thth values in degrees
#' @param wavelength  radiation wavelength in Angstrom
#'
#' @return d-spacings (numeric)
#' @export
thth2d <- function(thth, wavelength = 1.540562) {
   # Wavelengths:
   #    Ag-Ka1 wavelength=0.5594075
   #    Ag-Ka2 wavelength=0.563798
   #    Ag-Kb1 wavelength=0.497069
   #    Ag-Kb2 wavelength=0.497685
   #    Co-Ka1 wavelength=1.788965
   #    Co-Ka2 wavelength=1.792850
   #    Co-Kb1 wavelength=1.620790
   #    Cr-Ka1 wavelength=2.289700
   #    Cr-Ka2 wavelength=2.293606
   #    Cr-Kb1 wavelength=2.084870
   #    Cu-Ka1 wavelength=1.540562
   #    Cu-Ka2 wavelength=1.544398
   #    Cu-Kb1 wavelength=1.392218
   #    Fe-Ka1 wavelength=1.936042
   #    Fe-Ka2 wavelength=1.939980
   #    Fe-Kb1 wavelength=1.756610
   #    Ge-Ka1 wavelength=1.254054
   #    Ge-Ka2 wavelength=1.258011
   #    Ge-Kb1 wavelength=1.057300
   #    Ge-Kb2 wavelength=1.057830
   #    Mo-Ka1 wavelength=0.709300
   #    Mo-Ka2 wavelength=0.713590
   #    Mo-Kb1 wavelength=0.632288
   #    Mo-Kb2 wavelength=0.632860
   #    Ni-Ka1 wavelength=1.657910
   #    Ni-Ka2 wavelength=1.661747
   #    Ni-Kb1 wavelength=1.500135
   #    Zn-Ka1 wavelength=1.435155
   #    Zn-Ka2 wavelength=1.439000
   #    Zn-Kb1 wavelength=1.295250
   return (wavelength / (2 * sin(as.radians(thth))))
}
