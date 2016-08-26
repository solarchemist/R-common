##################################################
################ wavenum2length ##################
##################################################

wavenum2length <- function(wavenumber) {
   # Converts wavenumber (cm-1) to wavelength (nm)
   # Only valid for absolute wavenumbers,
   # NOT delta wavenumbers (ranges)
   # http://www.powerstream.com/inverse-cm.htm
	 wavelength <-
	    10E6 / wavenumber
   return(wavelength)
}
