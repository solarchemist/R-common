thth2d <- function(thth, wavelength = 1.540562) {
   # This function applies Bragg's law to calculate d-spacings from thth (n = 1)
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
   # Usage:
   # thth       : vector with thth values in degrees
   # wavelength : radiation wavelength in Ångström
   return (wavelength / (2 * sin(as.radians(thth))))
}