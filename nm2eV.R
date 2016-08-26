##################################################
#################### nm2eV #######################
##################################################
source(HomeByHost("/home/taha/chepec/chetex/common/R/sunlight/solarconstants.R"))

nm2eV <- function(nm) {
   # Converts wavelength in nm to energy in eV
   #
   sun.constants <- solar.constants()
   
   eV <- 
      sun.constants["h.eV", "value"] * 
      1E9 * sun.constants["c", "value"] / nm
   
   return(eV)
}
