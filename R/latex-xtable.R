#' Set the attributes for a generic xtable object
#'
#' This function helps you to set the attributes for an xtable
#' object. It returns an xtable object.
#'
#' @param xtobject      the xtable (table)
#' @param nxtnames      vector of names (column names)
#' @param nxtdigits     vector of digits (0 if column is non-numeric, numeric of desired number of digits otherwise)
#' @param nxtdisplay    vector of display format [see formatC(format=...)]
#' @param nxtalign      vector of LaTeX align (e.g., "l", "c", "r", "S[table-format=1.1]", ...)
#' @param caption.text  string for the LaTeX caption text
#' @param caption.label string for the LaTeX reference label
#'
#' @details
#' Sets names, digits, display, and align for the passed xtable object
#'
#' @return xtable object
#' @export
#'
#' @examples
#' \dontrun{
#' xtabWithAttributes <- GenericXtableSetAttributes(xtobject)
#' xtabWithAttributes <- GenericXtableSetAttributes(xtobject, nxtdigits = c(0, 2, 2, 4))
#' }
GenericXtableSetAttributes <- function(xtobject,
                                       nxtnames = NULL,
                                       nxtdigits = NULL,
                                       nxtdisplay = NULL,
                                       nxtalign = NULL,
                                       caption.text = "nxtcaption",
                                       caption.label = "tab:nxtlabel") {

   # remember to put all names inside "{}" if you use siunitx
   if (!is.null(nxtnames))   {names(xtobject)   <- nxtnames}
   # the prepended column due to "row.names"
   if (!is.null(nxtdigits))  {xtable::digits(xtobject)  <- c(0, nxtdigits)}
   if (!is.null(nxtdisplay)) {xtable::display(xtobject) <- c("s", nxtdisplay)}
   if (!is.null(nxtalign))   {xtable::align(xtobject)   <- c("l", nxtalign)}
   xtable::caption(xtobject) <- caption.text
   xtable::label(xtobject) <- caption.label
   #
   return (xtobject)
}


#' Set xtable header in LaTeX longtable format
#'
#' This function creates a longtable header assuming
#' that the LaTeX document will use the booktabs package.
#' This function should not be used together with \code{booktabs = TRUE}
#'
#' @param xtobject        xtable object (table)
#' @param caption.text    string for the LaTeX caption text
#' @param caption.label   string for the LaTeX reference label
#'
#' @return character string (with LaTeX escaping)
#' @export
LongtableXtableHeader <- function(xtobject, caption.text, caption.label) {
   ltxt.header <-
      paste(paste("\\caption{", caption.text, "}", sep = "", collapse = ""),
            paste("\\label{", caption.label, "}\\\\ ", sep = "", collapse = ""),
            "\\toprule ",
            attr(xtobject, "names")[1],
            paste(" &", attr(xtobject, "names")[2:length(attr(xtobject, "names"))], collapse = ""),
            "\\\\\\midrule ",
            "\\endfirsthead ",
            paste("\\multicolumn{",
                  ncol(xtobject),
                  "}{c}{{\\tablename\\ \\thetable{} -- continued from previous page}}\\\\ ",
                  sep = ""),
            "\\toprule ",
            attr(xtobject, "names")[1],
            paste("&", attr(xtobject, "names")[2:length(attr(xtobject, "names"))], collapse = ""),
            "\\\\\\midrule ",
            "\\endhead ",
            "\\midrule ",
            paste("\\multicolumn{",
                  as.character(ncol(xtobject)),
                  "}{r}{{Continued on next page}}\\\\ ",
                  sep = "", collapse = ""),
            "\\bottomrule \\endfoot ",
            "\\bottomrule \\endlastfoot ",
            collapse = "")
   return(ltxt.header)
}


#' Set xtable header in LaTeX tabular format
#'
#' This function should be used together with \code{booktabs = TRUE}.
#'
#' @param xtobject       xtable object (table)
#' @param names.custom   Use \code{names.custom} to make more complicated headers, e.g., multiple-row
#'
#' @return character string (with LaTeX escaping)
#' @export
TabularXtableHeader <- function(xtobject, names.custom = NULL) {
   if (is.null(names.custom)) {
      txt.header <-
         paste(attr(xtobject, "names")[1],
               paste(" &", attr(xtobject, "names")[2:length(attr(xtobject, "names"))], collapse = ""),
               "\\\\\n")
   } else {
      txt.header <- names.custom
   }
   return(txt.header)
}
