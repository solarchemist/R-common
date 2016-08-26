GenericXtableSetAttributes <- function(xtobject, 
                                       nxtnames = NULL, 
                                       nxtdigits = NULL, 
                                       nxtdisplay = NULL, 
                                       nxtalign = NULL,
                                       caption = "nxtcaption",
                                       label = "tab:nxtlabel") {
   #' @title Set the attributes for a generic xtable
   #'
   #' @description
   #' Sets attributes for the passed xtable object
   #'
   #' @details
   #' Sets names, digits, display, and align for the passed xtable object
   #'
   #' @param xtobject the xtable(table)
   #' @param nxtnames vector of names (column names)
   #' @param nxtdigits vector of digits (0 if column is non-numeric, numeric of desired number of digits otherwise)
   #' @param nxtdisplay vector of display format [see formatC(format=...)]
   #' @param nxtalign vector of LaTeX align (e.g., "l", "c", "r", "S[table-format=1.1]", ...)
   #' @examples
   #' GenericXtableSetAttributes(xtable(yourtable), nxtdigits = c(0, 2, 2, 4))
   #' @author Taha Ahmed <taha@@chepec.se>
   #' @return xtable

   # remember to put all names inside "{}" if you use siunitx
   if (!is.null(nxtnames))   {names(xtobject)   <- nxtnames}
   # the prepended column due to "row.names"
   if (!is.null(nxtdigits))  {digits(xtobject)  <- c(0, nxtdigits)}
   if (!is.null(nxtdisplay)) {display(xtobject) <- c("s", nxtdisplay)}
   if (!is.null(nxtalign))   {align(xtobject)   <- c("l", nxtalign)}
   caption(xtobject) <- caption
   label(xtobject) <- label
   #
   return (xtobject)
}
