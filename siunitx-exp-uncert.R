# Writing number with exponent (such as scientific notation) with uncertainty using siunitx

# Origin of problem, this kind of code:
# \SI[separate-uncertainty=true]{\Sexpr{formatC(cd.flux, format = "e", digits = 2)} \pm \Sexpr{formatC(cd.flux.error, format = "e", digits = 2)}}{\milli\coulomb\per\square\cm\per\second}
# makes siunitx throw the error:  
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !
# ! siunitx error: "misplaced-sign-token"
# ! 
# ! Misplaced sign token '\pm '.
# ! 
# ! See the siunitx documentation for further information.
# ! 
# ! For immediate help type H <return>.
# !...............................................  


# This is status-by-design according to Joseph Wright.
# http://tex.stackexchange.com/questions/123771/exponent-notation-in-siunitx

# But if the two numbers (number and uncertainty) have the same exponent, that is ok.
# So here we try to write a function that accepts two numbers, 
# and returns them written in a common exponent (as strings).

siunitx.uncertainty <- function(quantity, uncertainty, digits = 6) {
   # both arguments should be numeric
   
   # how to find common exponent for two numbers?
   
   # find exponent of quantity (tiopotensen for kvantiteten)
   quantity.exponent <- floor(log(abs(quantity), 10))
   
   # find coefficient of quantity
   # warning, numeric-to-string-to-numeric conversion ...
   quantity.coefficient <- 
      as.numeric(strsplit(formatC(quantity, format="e", digits=digits), "[Ee]-")[[1]][1])
   
   # construct return quantity string
   rquantity.string <- 
      paste0(formatC(quantity.coefficient, format="f", digits=digits), "e", quantity.exponent)
   
   
      
   # find exponent of uncertainty (tiopotensen for the uncertainty)
   uncertainty.exponent <- floor(log(abs(uncertainty), 10))
   
   # find coefficient of uncertainty
   # warning, numeric-to-string-to-numeric conversion ...
   uncertainty.coefficient <- 
      as.numeric(strsplit(formatC(uncertainty, format="e", digits=digits), "[Ee]-")[[1]][1])
   
   # adjust uncertainty to the same exponent as the quantity
   # express uncertainty with the same exponent as quantity 
   # (adjust number of uncertainty accordingly)
   runcertainty.exponent <- quantity.exponent
   runcertainty.coefficient <- uncertainty.coefficient * 10^(uncertainty.exponent - quantity.exponent)
   
   runcertainty.string <- 
      paste0(formatC(runcertainty.coefficient, format="f", digits=digits), "e", runcertainty.exponent)
      

   # create a string directly suitable for the siunitx \num{} command
   siunitx.string <- paste(quantity.coefficient, "\\pm", runcertainty.string)

   return(c(quantity = rquantity.string, 
            uncertainty = runcertainty.string,
            siunitx = siunitx.string))
}
   
   
   
   
   
   
   
   
   
   
   
   
   