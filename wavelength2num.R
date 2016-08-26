##################################################
################ wavelength2num ##################
##################################################

wavelength2num <- function(wavelength) {
   # Converts wavelength (nm) to wavenumber (cm-1)
   # Only valid for absolute wavelengths, 
   # NOT delta wavelengths (ranges)
   # http://www.powerstream.com/inverse-cm.htm
	wavenumber <-
		10E6 / wavelength
   return(wavenumber)
}
