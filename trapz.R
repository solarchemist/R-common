#####################################
############# trapz #################
#####################################
trapz <- function(x, y) {
   ## Description:
   ##   Performs a trapezoidal integration (approximate numerical integration
   ##   of the area under the curve defined by the x and y coordinate pairs.
   ##   See Wikipedia for more info on trapezoidal integration.  
   ## Usage:
   ##   trapz(x, y)
   ## Arguments:
   ##   x: vector (of length n)
   ##   y: vector (of length n)
   ## Return value:
   ##   vector of length (n - 1)
   #
   idx <- 2:length(x)
   return (as.double((x[idx] - x[idx - 1]) * (y[idx] + y[idx - 1])) / 2)
}
