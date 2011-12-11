source("/home/taha/chepec/chetex/common/R/common/LoadRData2Variable.R")

##################################################
############## SubstrateHistory ##################
##################################################
SubstrateHistory <- function(sampleid) {
   # 
   #
   matrix <- LoadRData2Variable("/home/taha/chepec/chetex/sample-matrix/sample-matrix-substrates.rda")
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
                                         which(names(sample.history) == "sampleid"), 
                                         which(names(sample.history) == "created"), 
                                         which(names(sample.history) == "project"))]
   # Save the empty column names as attribute to sample.history dataframe
   attr(sample.history, "empty.names") <- empty.names  
   
   return(sample.history)
}
