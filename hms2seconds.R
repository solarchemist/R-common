##################################################
################# hms2seconds ####################
##################################################
hms2seconds <- function(hms_vec) {
   ## Description:
   ##   Converts an hh:mm:ss time into seconds.
   ## Usage:
   ##   hms2seconds(hh:mm:ss)
   ## Arguments:
   ##     hms_vec: a character vector
   ## Value:
   ##   A numeric vector with the same number of elements
   ##   as the input vector
   #
   seconds <- rep(NA, length(hms_vec))
   for (i in 1:length(hms_vec)) {
      hms_str <- strsplit(hms_vec[i], ":")[[1]]
      # We assume hours:min:sec, anything else, throw an error
      if (length(hms_str) != 3) {
         error("Input must be formatted as hh:mm:ss")
      }
      seconds[i] <- 
         as.numeric(hms_str[1]) * 3600 + 
         as.numeric(hms_str[2]) * 60 + 
         as.numeric(hms_str[3])
   }
   return(seconds)
}
