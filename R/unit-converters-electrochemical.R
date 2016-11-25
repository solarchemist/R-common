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



#' Get standardised name of reference electrode
#'
#' Given a reference electrode label, this function returns its canonical name
#' (as defined by this package).
#' This function tries to match against as many variations as possible for each
#' reference electrode.
#'
#' @param refname string
#'
#' @return the canonical name or empty string
RefCanonicalName <- function(refname) {
   # scale names
   electrode.system <- list()
   electrode.system[["SHE"]] <-
      c("SHE",
        "standard hydrogen",
        "standard hydrogen electrode")
   electrode.system[["AgCl/Ag"]] <-
      c("AgCl/Ag",
        "Ag/AgCl",
        "AgCl",
        "silver-silver chloride",
        "silver chloride",
        "SSC") # saturated silver-silver chloride is sometimes abbreviated SSC
   electrode.system[["Hg2Cl2/Hg"]] <-
      c("Hg2Cl2/Hg",
        "Hg/Hg2Cl2",
        "Hg2Cl2",
        "calomel-mercury",
        "mercury-calomel",
        "SCE")
   electrode.system[["AVS"]] <-
      c("AVS",
        "vacuum",
        "vacuum scale",
        "absolute",
        "absolute scale",
        "absolute vacuum scale")
   electrode.system[["Li"]] <-
      c("Li",
        "Li/Li+",
        "Lithium")

   # to match the lowercase version, use tolower()
   # perhaps also replace hyphens and slashes with space?

   matches <-
      data.frame(electrode = names(electrode.system),
                 m = rep(0, length(electrode.system)),
                 stringsAsFactors = FALSE)

   # loop over electrode systems
   for (i in 1:length(electrode.system)) {
      # check for a match in any cell of this row,
      # also trying all lower-case and substituting symbols with spaces
      if (any(electrode.system[[i]] == refname) ||
          any(tolower(electrode.system[[i]]) == refname) ||
          any(gsub("[-/]", " ", electrode.system[[i]]) == refname)) {
         matches$m[i] <- 1
      }
   }

   # if everything went as expected we should have just one match
   if (sum(matches$m) != 1) {
      # something wrong (should probably add warn/error here)
      # for now, just return empty string
      return("")
   } else {
      return(matches$electrode[which(matches$m == 1)])
   }
}




#' Potentials as SHE
#'
#' This function just outputs a tidy dataframe with potential vs SHE for
#' different scales, electrolytes, concentrations, and temperatures.
#' Using data from literature.
#'
#' @return tidy dataframe with the following columns
#'    \tabular{ll}{
#'    \code{electrode}     \tab reference electrode \cr
#'    \code{electrolyte}   \tab electrolyte \cr
#'    \code{conc.num}      \tab concentration of electrolyte, mol/L \cr
#'    \code{conc.string}   \tab concentration of electrolyte, as string, may also note temperature at which conc \cr
#'    \code{temp}          \tab temperature / degrees Celsius \cr
#'    \code{SHE}           \tab potential vs SHE / volt \cr
#'    \code{sid}           \tab set id, just for housekeeping inside this function \cr
#'    \code{reference}     \tab BibTeX reference \cr
#'    \code{dEdT}          \tab temperature coefficient / volt/kelvin \cr
#'    }
#' @export
potentials.as.SHE <- function() {
   # scale name should be one of canonical (see RefCanonicalName)

   # follow the convention of "each row one observation" (at different temperatures)
   # all potentials vs SHE

   potentials <-
      as.data.frame(matrix(data =
                              # electrode # electrolyte # conc/M # conc label # temp # pot vs SHE # set id # ref
                              c("AgCl/Ag",   "KCl(aq)", "3.5", "3.5M at 25C", "10", "0.215", "1", "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)", "3.5", "3.5M at 25C", "15", "0.212", "1", "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)", "3.5", "3.5M at 25C", "20", "0.208", "1", "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)", "3.5", "3.5M at 25C", "25", "0.205", "1", "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)", "3.5", "3.5M at 25C", "30", "0.201", "1", "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)", "3.5", "3.5M at 25C", "35", "0.197", "1", "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)", "3.5", "3.5M at 25C", "40", "0.193", "1", "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)", "4.2", "saturated",   "10", "0.214", "2", "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)", "4.2", "saturated",   "15", "0.209", "2", "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)", "4.2", "saturated",   "20", "0.204", "2", "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)", "4.2", "saturated",   "25", "0.199", "2", "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)", "4.2", "saturated",   "30", "0.194", "2", "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)", "4.2", "saturated",   "35", "0.189", "2", "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)", "4.2", "saturated",   "40", "0.184", "2", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "0.1", "0.1M at 25C", "10", "0.336", "3", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "0.1", "0.1M at 25C", "15", "0.336", "3", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "0.1", "0.1M at 25C", "20", "0.336", "3", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "0.1", "0.1M at 25C", "25", "0.336", "3", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "0.1", "0.1M at 25C", "30", "0.335", "3", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "0.1", "0.1M at 25C", "35", "0.334", "3", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "0.1", "0.1M at 25C", "40", "0.334", "3", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "1.0", "1.0M at 25C", "10", "0.287", "4", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "1.0", "1.0M at 25C", "20", "0.284", "4", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "1.0", "1.0M at 25C", "25", "0.283", "4", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "1.0", "1.0M at 25C", "30", "0.282", "4", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "1.0", "1.0M at 25C", "40", "0.278", "4", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "3.5", "3.5M at 25C", "10", "0.256", "5", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "3.5", "3.5M at 25C", "15", "0.254", "5", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "3.5", "3.5M at 25C", "20", "0.252", "5", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "3.5", "3.5M at 25C", "25", "0.250", "5", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "3.5", "3.5M at 25C", "30", "0.248", "5", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "3.5", "3.5M at 25C", "35", "0.246", "5", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "3.5", "3.5M at 25C", "40", "0.244", "5", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "4.2", "saturated",   "10", "0.254", "6", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "4.2", "saturated",   "15", "0.251", "6", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "4.2", "saturated",   "20", "0.248", "6", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "4.2", "saturated",   "25", "0.244", "6", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "4.2", "saturated",   "30", "0.241", "6", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "4.2", "saturated",   "35", "0.238", "6", "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)", "4.2", "saturated",   "40", "0.234", "6", "Sawyer1995",
                                "AVS",       "",        "",     "",           "25", "-4.44", "7", "Trasatti1986"),
                           ncol = 8,
                           byrow = TRUE), stringsAsFactors = FALSE)

   colnames(potentials) <-
      c("electrode",
        "electrolyte",
        "conc.num",
        "conc.string",
        "temp",
        "SHE",
        "sid",
        "reference")

   # convert these columns to type numeric
   potentials[, c("conc.num", "temp", "SHE")] <-
      as.numeric(as.character(unlist(potentials[, c("conc.num", "temp", "SHE")])))

   # make room for a dE/dT column
   potentials$dEdT <- as.numeric(NA)

   # calculate temperature coefficient (dE/dT) for each scale and concentration (ie. set id)
   for (s in 1:length(unique(potentials$sid))) {
      # sid column eas added to data just to make this calculation here easier
      subspot <- potentials[which(potentials$sid == unique(potentials$sid)[s]), ]
      # a linear fit will give us temperature coefficient as slope
      lm.subspot <- stats::lm(SHE ~ temp, data = subspot)
      potentials[which(potentials$sid == unique(potentials$sid)[s]), "dEdT"] <-
         lm.subspot$coefficients[2]
   }

   return(potentials)
}




#' Convert from electrochemical or electronic scale to SHE
#'
#' @param potential in the original scale, V or eV
#' @param scale name of the original scale
#' @param concentration of electrolyte in mol/L, or as the string "saturated"
#' @param temperature of system in degrees Celsius
#' @param as.SHE.data dataframe with dataset
#'
#' @return potential in SHE scale
#' @export
as.SHE <- function(potential,
                   scale,
                   concentration = "saturated",
                   temperature = 25,
                   as.SHE.data = potentials.as.SHE()) {

   # if the supplied temperature does not exist in the data, this function will attempt to interpolate
   # note that concentration has to match, no interpolation is attempted for conc

   if (RefCanonicalName(scale) == "") {
      warning("as.SHE(): Sorry, you have supplied an unrecognised electrode scale.")
      return(NA)
   }

   # there is the simple case of
   if (RefCanonicalName(scale) == "SHE") {
      warning("This function can only convert from scales other than SHE!")
      return(NA)
   }

   # AVS needs special consideration
   if (RefCanonicalName(scale) == "AVS") {
      # reset arg concentration
      concentration <- ""
      # second, since AVS scale goes in the opposite direction to the electrochemical scales
      # we will define our own function
      negifavs <- function(a, b) {
         a - b
      }
   } else {
      # we will define the same function differently for
      # the case we're not dealing with AVS
      negifavs <- function(a, b) {
         a + b
      }
   }

   if (is.character(concentration)) {
      # supplied concentration is character string
      subspot <-
         subset(subset(as.SHE.data,
                       electrode == RefCanonicalName(scale)),
                conc.string == concentration)
      # if either "scale" or "concentration" are not found in the data, subspot will contain zero rows
      if (dim(subspot)[1] == 0) {
         warning("as.SHE(): Supplied scale or concentration does not exist in data. Returning NA.")
         return(NA)
      }
      # so far, we have
      # scale: checked!
      # concentration: checked!
      # only temperature remains to be handled
      # temperature value could happen to match a value in the data, or lie somewhere in between
      # note: we will not allow extrapolation
      if (!any(subspot$temp == temperature)) {
         # if sought temperature is not available in dataset, check that it falls inside
         if ((temperature < max(subspot$temp)) && (temperature > min(subspot$temp))) {
            # within dataset range, do linear interpolation
            lm.subspot <- stats::lm(SHE ~ temp, data = subspot)
            # interpolated temperature, calculated based on linear regression
            # (more accurate than simple linear interpolation with approx())
            potinterp <-
               lm.subspot$coefficients[2] * temperature + lm.subspot$coefficients[1]
            ### CALC RETURN POTENTIAL
            return(negifavs(potinterp, potential))
         } else {
            # outside dataset range, warning and return NA (we don't extrapolate)
            warning("as.SHE(): the temperature you requested falls outside data range.")
            return(NA)
         }
      } else {
         # requested temperature does exist in dataset
         ### CALC RETURN POTENTIAL
         return(negifavs(subset(subspot, temp == temperature)$SHE, potential))
      }
   # outer-most if-else
   } else {
      # supplied concentration is numeric
      # note: all code inside this else is the same as inside the if,
      # just for the case of numeric concentration
      subspot <-
         subset(subset(as.SHE.data,
                       electrode == RefCanonicalName(scale)),
                conc.num == concentration)
      # if either "scale" or "concentration" are not found in the data, subspot will contain zero rows
      if (dim(subspot)[1] == 0) {
         warning("as.SHE(): Supplied scale or concentration does not exist in data. Returning NA.")
         return(NA)
      }
      if (!any(subspot$temp == temperature)) {
         # if sought temperature is not available in dataset, check that it falls inside
         if ((temperature < max(subspot$temp)) && (temperature > min(subspot$temp))) {
            # within dataset range, do linear interpolation
            lm.subspot <- stats::lm(SHE ~ temp, data = subspot)
            # interpolated temperature, calculated based on linear regression
            # (more accurate than simple linear interpolation with approx())
            potinterp <-
               lm.subspot$coefficients[2] * temperature + lm.subspot$coefficients[1]
            ### CALC RETURN POTENTIAL
            return(negifavs(potinterp, potential))
         } else {
            # outside dataset range, warning and return NA (we don't extrapolate)
            warning("as.SHE(): the temperature you requested falls outside data range.")
            return(NA)
         }
      } else {
         # requested temperature does exist in dataset
         ### CALC RETURN POTENTIAL
         return(negifavs(subset(subspot, temp == temperature)$SHE, potential))
      }

   }
}









#' ConvertRefPotEC
#'
#' This function does the heavy lifting.
#' Converts from an electrochemical reference scale into another.
#' SHE:     standard hydrogen electrode
#' Ag/AgCl: silver silver-chloride electrode (3M KCl)
#' SCE:     saturated calomel electrode
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
   SHE0 <-
      data.frame(matrix(refpotatSHEzero,
                        ncol = length(refpotatSHEzero),
                        byrow = T))
   refpotmtx <- matrix(NA, length(SHE0), length(SHE0))
   refpotmtx[,1] <- matrix(as.matrix(SHE0), ncol = 1, byrow = T)
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
   bool.flags <-
      as.data.frame(matrix(0,
                           nrow = length(scale.names),
                           ncol = 2))
   names(bool.flags) <- c("argref", "valueref")
   row.names(bool.flags) <- names(scale.names)

   # argrefscale
   # Check that argrefscale is valid character mode
   # ...

   # steps through all scale names, "row-by-row",
   # looking for any cell matching "argrefscale" string
   # if found, save the position of that refelectrode (in scale.names) to
   # that row and "argref" column of bool.flags
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
