##################################################
#################### eV2nm #######################
##################################################
eV2nm <- function(eV) {
   # Converts energy in eV to wavelength in nm
   #
   # Define some constants needed for the calculations
   Plancks.constant <- 4.135667516E-15 # \electron\volt\per\second
   speed.of.light   <- 299792458       # \meter\per\second
   
   nm <- Plancks.constant * 1E9 * speed.of.light / eV
   return(nm)
}
