#' AVS -> SHE
#'
#' Converts from absolute vacuum scale (AVS) to SHE scale
#'
#' @param avs Potential in AVS scale
#'
#' @return potential in SHE scale (numeric)
#' @export
AVS2SHE <- function(avs) {
   she <- -(4.5 + avs)
   return(she)
}


#' SHE -> AVS
#'
#' Converts from SHE scale to absolute vacuum (AVS) scale
#'
#' @param she Potential in SHE scale
#'
#' @return potential in AVS scale (numeric)
#' @export
SHE2AVS <- function(she) {
   avs <- -(4.5 + she)
   return(avs)
}


#' ConvertRefPotEC
#'
#' This function does the heavy lifting.
#' Converts from an electrochemical reference scale into another.
#' SHE:     standard hydrogen electrode scale
#' Ag/AgCl: silver silver-chloride electrode scale (3M KCl)
#' SCE:     standard calomel scale
#'
#' @param argpotential    potential (numeric)
#' @param argrefscale     input reference scale (character string)
#' @param valuerefscale   output reference scale (character string)
#'
#' @return potential in output reference scale (numeric)
ConvertRefPotEC <- function(argpotential, argrefscale, valuerefscale) {
   ##### Add more reference electrodes here >>
   refpotatSHEzero <- c(     0,     -0.21,  -0.24,        3)
   refrownames     <- c( "SHE", "Ag/AgCl",  "SCE", "Li/Li+")
   refcolnames     <- c("SHE0",   "AgCl0", "SCE0",    "Li0")
   ##### Add more reference electrodes here <<
   #
   SHE0 <- data.frame(matrix(refpotatSHEzero, ncol=length(refpotatSHEzero), byrow=T))
   refpotmtx <- matrix(NA, length(SHE0), length(SHE0))
   refpotmtx[,1] <- matrix(as.matrix(SHE0), ncol=1, byrow=T)
   for (c in 2:length(SHE0)) {
      # loop over columns (except the first)
      for (r in 1:length(SHE0)) {
         # loop over rows
         refpotmtx[r, c] <- refpotmtx[r, 1] - refpotmtx[c, 1]
      }
   }
   refpotdf <- as.data.frame(refpotmtx)
   names(refpotdf) <- refcolnames
   row.names(refpotdf) <- refrownames
   ## So far we have made a matrix of all the possible combinations,
   ## given the vector refpotatSHEzero. The matrix is not strictly necessary,
   ## but it may prove useful later. It does.
   #
   # Match argrefscale to the refrownames
   argmatch <- match(argrefscale, refrownames, nomatch = 0)
   # Match valuerefscale to the refrownames
   valuematch <- match(valuerefscale, refrownames, nomatch = 0)
   # We simply assume that the match was well-behaved
   valuepotential <- argpotential + refpotdf[valuematch, argmatch]
   # Check that arg and value electrodes are within bounds for a match
   if (argmatch == 0 || valuematch == 0) {
      # No match
      # Perform suitable action
      message("Arg out of bounds in call to ConvertRefPot")
      valuepotential <- NA
   }
   return(valuepotential)
}


#' Convert from one electrochemical scale to another
#'
#' @param argpotential    potential (numeric)
#' @param argrefscale     input reference scale (char string)
#' @param valuerefscale   output reference scale (char string)
#'
#' @return potential in output reference scale (numeric)
#' @export
ConvertRefPot <- function(argpotential, argrefscale, valuerefscale) {
   # You should check that argpotential is valid numeric

   #  IDEA: make a matrix out of these (scale names and flags)

   # Valid scales
   scale.names <- list()
   scale.names[["SHE"]] <- c("SHE", "NHE", "she", "nhe")
   scale.names[["AgCl"]] <- c("Ag/AgCl", "AgCl", "ag/agcl", "agcl")
   scale.names[["SCE"]] <- c("SCE", "sce")
   scale.names[["Li"]] <- c("Li/Li+", "Li", "Li+", "li", "li+", "li/li+")
   scale.names[["AVS"]] <- c("AVS", "avs")

   # Set flags
   bool.flags <- as.data.frame(matrix(0, nrow = length(scale.names), ncol = 2))
   names(bool.flags) <- c("argref", "valueref")
   row.names(bool.flags) <- names(scale.names)

   # argrefscale
   # Check that argrefscale is valid character mode
   # ...

   for (j in 1:length(row.names(bool.flags))) {
      if (any(scale.names[[row.names(bool.flags)[j]]] == argrefscale)) {
         bool.flags[row.names(bool.flags)[j], "argref"] <- j
      }
   }


   # valuerefscale
   # Check that valuerefscale is valid character mode
   # ...

   for (k in 1:length(row.names(bool.flags))) {
      if (any(scale.names[[row.names(bool.flags)[k]]] == valuerefscale)) {
         bool.flags[row.names(bool.flags)[k], "valueref"] <- k
      }
   }

   # Depending on which flags are set, call the corresponding function

   decision.vector <- colSums(bool.flags)

   # Check if both scales are the same (no conversion needed). If so, abort gracefully.
   # ...

   if (decision.vector["argref"] == 5 || decision.vector["valueref"] == 5) {
      # AVS is requested, deal with it it
      if (decision.vector["argref"] == 5) {
         # Conversion _from_ AVS
         rnpotential <- ConvertRefPotEC(AVS2SHE(argpotential),
                                        "SHE",
                                        scale.names[[decision.vector["valueref"]]][1])
      }
      if (decision.vector["valueref"] == 5) {
         # Conversion _to_ AVS
         rnpotential <- SHE2AVS(ConvertRefPotEC(argpotential,
                                                scale.names[[decision.vector["argref"]]][1],
                                                "SHE"))
      }
   } else {
      rnpotential <- ConvertRefPotEC(argpotential,
                                     scale.names[[decision.vector["argref"]]][1],
                                     scale.names[[decision.vector["valueref"]]][1])
   }
   return(rnpotential)
}
