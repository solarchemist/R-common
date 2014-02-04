TabularXtableHeader <- function(xtobject, names.custom = NULL) {
   # use names.custom to make more complicated headers, e.g. multiple-row
   # should be used together with booktabs = TRUE
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
