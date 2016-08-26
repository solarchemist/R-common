##################################################
################## capitalize ####################
##################################################
capitalize <- function(x) {
   ## Description:
   ##   Capitalizes the first letter of a string
   ##   == This function was inspired by the function supplied in the base R doc for chartr()
   ## Usage:
   ##   capitalize(string)
   ## Arguments:
   ##   x: a string or vector of strings
   ## Value:
   ##   A string or vector of strings
   #
   paste(toupper(substring(x, 1, 1)), substring(x, 2), 
         sep = "")
}
