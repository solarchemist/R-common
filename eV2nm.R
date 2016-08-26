##################################################
#################### eV2nm #######################
##################################################
source(HomeByHost("/home/taha/chepec/chetex/common/R/sunlight/solarconstants.R"))

eV2nm <- function(eV) {
   # Converts energy in eV to wavelength in nm
   #
   sun.constants <- solar.constants()
   
   nm <- 
      sun.constants["h.eV", "value"] * 
      1E9 * sun.constants["c", "value"] / eV
   
   return(nm)
}
