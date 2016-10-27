#' Create sampleid from path to experiment datafile
#'
#' Returns a "unique" sample ID when supplied with a path to an experimental file.
#' The second arg is optional, defaults to "old" behaviour,
#' but can be set to "dirname" for another behaviour.
#' The second arg was added so as not to break older code.
#'
#' @param pathexpfile     path to an experiment datafile
#' @param implementation  defaults to "old" behaviour, can also be set to "dirname"
#'
#' @return a sampleid (character string)
#' @export
ProvideSampleId <- function (pathexpfile, implementation = "filename") {
   ## Note to myself: the sample ID must derive directly from the file or path.
   if (implementation == "dirname") {
      # basename(dirname()) returns the name of the lowest sub-directory
      # split()[[1]][2] splits the dirname at the hyphen and returns the sampleid
      sampleid <- strsplit(x = basename(dirname(pathexpfile)),
                           split = "-")[[1]][2]
   } else {
      # basename() returns the filename sans path
      # sub() returns the filename sans extension
      sampleid <- sub("\\.[\\w]+$", "", basename(pathexpfile), perl = TRUE)
   }

   return(sampleid)
}


#' Display the history of a substrate (sampleid)
#'
#' @param sampleid    string
#' @param matrix.rda  path to rdata file containing the sample matrix
#'
#' @return a dataframe
#' @export
SubstrateHistory <- function(sampleid, matrix.rda) {
   matrix <- LoadRData2Variable(matrix.rda)
   # Extract the rows pertaining to the current sampleid
   sample.history <- matrix[which(matrix$sampleid == sampleid), ]
   # Loops remove all "\\labreport{...}" strings (they make the table too wide otherwise)
   for (j in 1:dim(sample.history)[1]) {
      for (k in 1:dim(sample.history)[2]) {
         sample.history[j, k] <- gsub("^\\\\labreport.*?\\}\\{", "", sample.history[j, k])
         sample.history[j, k] <- gsub("\\}$", "", sample.history[j, k])
      }
   }
   # Find empty columns and collect their column indices
   empty.cols <- matrix(data = FALSE, nrow = 1, ncol = dim(sample.history)[2])
   for (k in 1:dim(sample.history)[2]) {
      if (paste(sample.history[, k], collapse = "") == "") {
         empty.cols[, k] <- TRUE
      }
   }
   # Save the names of the empty columns
   empty.names <- names(sample.history)[which(empty.cols)]
   # Remove the identified empty columns plus those columns deemed unwanted
   sample.history <- sample.history[, -c(which(empty.cols == TRUE),
                                         #which(names(sample.history) == "sampleid"),
                                         which(names(sample.history) == "created"),
                                         which(names(sample.history) == "project"))]
   # Save the empty column names as attribute to sample.history dataframe
   attr(sample.history, "empty.names") <- empty.names

   return(sample.history)
}
