##################################################
#################### nm2eV #######################
##################################################
nm2eV <- function(nm) {
   # Converts wavelength in nm to energy in eV
   #
   # Define some constants needed for the calculations
   Plancks.constant <- 4.135667516E-15 # \electron\volt\per\second
   speed.of.light   <- 299792458       # \meter\per\second
   
   eV <- Plancks.constant * 1E9 * speed.of.light / nm
   return(eV)
}
