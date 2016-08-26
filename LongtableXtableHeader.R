LongtableXtableHeader <- function(xtobject, caption.text, caption.label) {
   # this function uses the \booktabs package
   # should NOT be used together with booktabs = TRUE
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
