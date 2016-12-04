#' AVS -> SHE
#'
#' Converts from absolute vacuum scale (AVS) to SHE scale
#'
#' @param avs Potential in AVS scale
#'
#' @return potential in SHE scale (numeric)
#' @export
AVS2SHE <- function(avs) {
   .Deprecated("as.SHE")
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
   .Deprecated("as.SHE")
   avs <- -(4.5 + she)
   return(avs)
}



#' Get standardised name of reference electrode
#'
#' Given a reference electrode label, this function returns its canonical name
#' (as defined by this package).
#' This function tries to match against as many variations as possible for each
#' reference electrode.
#' The entire point of this function is to decrease the mental load on the user
#' by not requiring them to remember a particular label or name for each reference
#' electrode, instead almost any sufficiently distinct label or string will still
#' be correctly identified.
#'
#' @param refname string or a vector of strings
#'
#' @return vector with corresponding "canonical" name or empty string (if none found)
#' @export
RefCanonicalName <- function(refname) {
   # scale names
   electrode.system <- list()
   electrode.system[["SHE"]] <-
      c("SHE",
        "Standard hydrogen",
        "Standard hydrogen electrode")
   electrode.system[["AgCl/Ag"]] <-
      c("AgCl/Ag",
        "Ag/AgCl",
        "AgCl",
        "Silver-Silver chloride",
        "Silver chloride",
        "SSC") # Sometimes used abbr. for Saturated Silver Chloride
   electrode.system[["Hg2Cl2/Hg"]] <-
      c("Hg2Cl2/Hg",
        "Hg/Hg2Cl2",
        "Hg2Cl2",
        "Calomel-Mercury",
        "Mercury-Calomel",
        "SCE")
   electrode.system[["AVS"]] <-
      c("AVS",
        "Vacuum",
        "Vacuum scale",
        "Absolute",
        "Absolute scale",
        "Absolute vacuum scale")
   electrode.system[["Li"]] <-
      c("Li",
        "Li/Li+",
        "Lithium")
   electrode.system[["Na"]] <-
      c("Na",
        "Na+/Na",
        "Na/Na+",
        "Sodium")
   electrode.system[["Mg"]] <-
      c("Mg",
        "Mg2+/Mg",
        "Mg/Mg2+",
        "Magnesium")

   # defining refname in this manner makes sure to get all possible combinations
   # but there might be a number of duplicates, but those we can
   # get rid of in the next step
   electrode <-
      data.frame(refname =
                    # here we create lower-case version of electrode.system,
                    # a version with symbols (-/) subbed with spaces,
                    # and a lower-case with symbols subbed with spaces
                    c(unname(unlist(electrode.system)),
                      tolower(unname(unlist(electrode.system))),
                      gsub("[-/]", " ", unname(unlist(electrode.system))),
                      gsub("[-/]", " ", tolower(unname(unlist(electrode.system))))),
                 refcanon =
                    rep(sub("[0-9]$", "", names(unlist(electrode.system))),
                        4), # this number needs to equal number of elements in c() above!
                 stringsAsFactors = FALSE)
   # detect and remove duplicates
   electrode <-
      electrode[!duplicated(electrode$refname),]
   # reset row numbering in dataframe just for good measure
   row.names(electrode) <- 1:dim(electrode)[1]

   # pre-allocate the return vector
   refcanon <- rep("", length(refname))
   # now all we have to do is check each user-submitted refname against
   # electrode$refname and return the value on the same row but next column
   for (i in 1:length(refname)) {
      refcanon[i] <-
         electrode$refcanon[which(electrode$refname == refname[i])]
   }

   return(refcanon)
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
                              c("AgCl/Ag",   "NaCl(aq)", "5.9", "saturated",   "25", "0.2630",  "9",  "CRC 97th ed., 97-05-22",
                                "AgCl/Ag",   "KCl(aq)",  "3.5", "3.5M at 25C", "10", "0.215",   "1",  "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)",  "3.5", "3.5M at 25C", "15", "0.212",   "1",  "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)",  "3.5", "3.5M at 25C", "20", "0.208",   "1",  "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)",  "3.5", "3.5M at 25C", "25", "0.205",   "1",  "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)",  "3.5", "3.5M at 25C", "30", "0.201",   "1",  "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)",  "3.5", "3.5M at 25C", "35", "0.197",   "1",  "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)",  "3.5", "3.5M at 25C", "40", "0.193",   "1",  "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)",  "4.2", "saturated",   "10", "0.214",   "2",  "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)",  "4.2", "saturated",   "15", "0.209",   "2",  "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)",  "4.2", "saturated",   "20", "0.204",   "2",  "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)",  "4.2", "saturated",   "25", "0.199",   "2",  "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)",  "4.2", "saturated",   "30", "0.194",   "2",  "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)",  "4.2", "saturated",   "35", "0.189",   "2",  "Sawyer1995",
                                "AgCl/Ag",   "KCl(aq)",  "4.2", "saturated",   "40", "0.184",   "2",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "0.1", "0.1M at 25C", "10", "0.336",   "3",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "0.1", "0.1M at 25C", "15", "0.336",   "3",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "0.1", "0.1M at 25C", "20", "0.336",   "3",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "0.1", "0.1M at 25C", "25", "0.336",   "3",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "0.1", "0.1M at 25C", "30", "0.335",   "3",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "0.1", "0.1M at 25C", "35", "0.334",   "3",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "0.1", "0.1M at 25C", "40", "0.334",   "3",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "1.0", "1.0M at 25C", "10", "0.287",   "4",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "1.0", "1.0M at 25C", "20", "0.284",   "4",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "1.0", "1.0M at 25C", "25", "0.283",   "4",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "1.0", "1.0M at 25C", "30", "0.282",   "4",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "1.0", "1.0M at 25C", "40", "0.278",   "4",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "3.5", "3.5M at 25C", "10", "0.256",   "5",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "3.5", "3.5M at 25C", "15", "0.254",   "5",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "3.5", "3.5M at 25C", "20", "0.252",   "5",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "3.5", "3.5M at 25C", "25", "0.250",   "5",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "3.5", "3.5M at 25C", "30", "0.248",   "5",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "3.5", "3.5M at 25C", "35", "0.246",   "5",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "3.5", "3.5M at 25C", "40", "0.244",   "5",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "4.2", "saturated",   "10", "0.254",   "6",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "4.2", "saturated",   "15", "0.251",   "6",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "4.2", "saturated",   "20", "0.248",   "6",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "4.2", "saturated",   "25", "0.244",   "6",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "4.2", "saturated",   "30", "0.241",   "6",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "4.2", "saturated",   "35", "0.238",   "6",  "Sawyer1995",
                                "Hg2Cl2/Hg", "KCl(aq)",  "4.2", "saturated",   "40", "0.234",   "6",  "Sawyer1995",
                                "AVS",       "",         "",    "",            "25", "-4.44",   "7",  "Trasatti1986",
                                "SHE",       "",         "",    "",       "-273.15", "0.00",    "8",  "Inzelt2013",
                                "SHE",       "",         "",    "",             "0", "0.00",    "8",  "Inzelt2013",
                                "SHE",       "",         "",    "",            "25", "0.00",    "8",  "Inzelt2013",
                                # arbitrary max T=580C (temp at which sodalime glass loses rigidity)
                                "SHE",       "",         "",    "",           "580", "0.00",    "8",  "Inzelt2013",
                                "Li",        "",         "1.0", "1.0M at 25C", "25", "-3.0401", "10", "CRC 97th ed., 97-05-22",
                                "Na",        "",         "1.0", "1.0M at 25C", "25", "-2.71",   "11", "CRC 97th ed., 97-05-22",
                                "Mg",        "",         "1.0", "1.0M at 25C", "25", "-2.372",  "12", "CRC 97th ed., 97-05-22"),
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

   # calculate temperature coefficient (dE/dT) for each scale, concentration, and electrolyte (ie. set id)
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
#' Convert an arbitrary number of potentials against any known electrochemical
#' scale (or the electronic vacuum scale) to potential vs SHE.
#'
#' @param potential potential in volt
#' @param scale name of the original scale
#' @param electrolyte optional, specify electrolyte solution, e.g., "KCl(aq)". Must match value in \code{as.SHE.data$electrolyte}.
#' @param concentration of electrolyte in mol/L, or as the string "saturated"
#' @param temperature of system in degrees Celsius
#' @param as.SHE.data dataframe with dataset
#'
#' @return potential in SHE scale
#' @export
as.SHE <- function(potential,
                   scale,
                   electrolyte = "",
                   concentration = "saturated",
                   temperature = 25,
                   as.SHE.data = potentials.as.SHE()) {

   # if the supplied temperature does not exist in the data, this function will attempt to interpolate
   # note that concentration has to match, no interpolation is attempted for conc

   # make this work for arbitrary-length vectors of potential and scale
   # make sure potential and scale args have the same length
   if (length(potential) == 0 | length(scale) == 0) {
      stop("Arguments potential or scale cannot be empty!")
   } else if (length(potential) != length(scale)) {
      stop("Arguments potential and scale must have equal number of elements")
   }

   arglength <- length(potential)
   # make the args concentration, temperature and electrolyte this same length,
   # unless the user supplied them (only necessary for > 1)
   if (arglength > 1) {
      # handle two cases:
      # 1. user did not touch concentration, temperature and electrolyte args.
      #    Assume they forgot and reset their length and print a message
      # 2. user did change concentration or temperature or electrolyte, but still failed to
      #    ensure length equal to arglength. In this case, abort.
      # note: we can get the default value set in the function call using formals()
      if (identical(concentration, formals(as.SHE)$concentration) &
          identical(temperature, formals(as.SHE)$temperature) &
          identical(electrolyte, formals(as.SHE)$electrolyte)) {
         # case 1
         # message("NOTE: default concentration and temperature values used for all potentials and scales.")
         message(paste0("Default concentration (", formals(as.SHE)$concentration, "), temperature (", formals(as.SHE)$temperature, "C) used for all supplied potential and scale values."))
         concentration <- rep(concentration, arglength)
         temperature <- rep(temperature, arglength)
         electrolyte <- rep(electrolyte, arglength)
      } else {
         # case 2
         stop("Concentration, temperature and electrolyte arguments must have the same number of elements as potential and scale!")
      }
   }

   ## we can now safely assume that length(<args>) == arglength
   # place args into a single dataframe
   # this way, we can correlate columns to each other by row
   df <-
      data.frame(potential = potential,
                 scale = RefCanonicalName(scale),
                 electrolyte = electrolyte,
                 concentration = concentration,
                 temperature = temperature,
                 stringsAsFactors = FALSE)
   # add column to keep track of vacuum scale
   df$vacuum <- as.logical(FALSE)
   # add column to hold calc potential vs SHE
   df$SHE <-  as.numeric(NA)

   # SHE scale special considerations
   # 1. concentration is constant
   if (any(df$scale == RefCanonicalName("SHE"))) {
      df$concentration[which(df$scale == RefCanonicalName("SHE"))] <- ""
      df$electrolyte[which(df$scale == RefCanonicalName("SHE"))] <- ""
   }

   # AVS scale special considerations
   # 1. concentration is meaningless
   # 2. direction is opposite of electrochemical scales, requiring change of sign
   if (any(df$scale == RefCanonicalName("AVS"))) {
      # concentration is meaningless for AVS (no electrolyte)
      # so for those rows, we'll reset it
      df$concentration[which(df$scale == RefCanonicalName("AVS"))] <- ""
      df$electrolyte[which(df$scale == RefCanonicalName("AVS"))] <- ""
      df$vacuum[which(df$scale == RefCanonicalName("AVS"))] <- TRUE
   }

   # now just work our way through df, line-by-line to determine potential as SHE
   # all necessary conditions should be recorded right here in df
   for (p in 1:dim(df)[1]) {
      if (is.character(df$concentration[p])) {
         subset.SHE.data <-
            subset(subset(as.SHE.data, conc.string == df$concentration[p]),
                   electrode == df$scale[p])
      } else {
         subset.SHE.data <-
            subset(subset(as.SHE.data, conc.num == df$concentration[p]),
                   electrode == df$scale[p])
      }

      # use KCl(aq) as default to avoid aborting
      # (good assumption at this point, as we always have KCl for the cases
      #  where an electrode system has more than one electrolyte)
      default.electrolyte <- "KCl(aq)"
      # If this subset contains more than one unique electrolyte (e.g., NaCl and KCl)
      # the user MUST have made a choice (in the "electrolyte" argument) that results
      # in a single electrolyte remaining, or else we will warn and abort
      if (length(unique(subset.SHE.data$electrolyte)) > 1) {
         # data (in subset.SHE.data) contains more than one electrolyte
         # if user did not change electrolyte arg value, use default and issue warning
         if (identical(electrolyte, formals(as.SHE)$electrolyte)) {
            warning(paste0("You did not specify an electrolyte, but more than one ",
                           "is available for E = ", df$potential[p], " V vs ", df$scale[p], ".\n",
                           "Using electrolyte", default.electrolyte))
            subset.SHE.data <-
               subset(subset.SHE.data, electrolyte == default.electrolyte)
         } else {
            # else the user did change the electrolyte arg, use the user's value
            subset.SHE.data <-
               subset.SHE.data[which(subset.SHE.data$electrolyte == electrolyte), ]
            print(subset.SHE.data)
            # stop if the resulting dataframe contains no rows
            if (dim(subset.SHE.data)[1] == 0) {
               stop("Your choice of electrolyte does not match any data!")
            }
         }
      } else {
         # data only contains one electrolyte
         # just check that it matches whatever the user supplied, if not,
         # issue a warning (but don't abort, typically the user did not set it
         # because they don't care and want whatever is in the data)
         if (unique(subset.SHE.data$electrolyte) != electrolyte) {
            warning(paste0("The requested electrolyte: ",
                           ifelse(electrolyte == "", "<none specified>", electrolyte),
                           " was not found for E = ", df$potential[p], " V vs ", df$scale[p], ".\n",
                           "My data only lists one electrolyte for that scale - return value calculated on that basis."))
            subset.SHE.data <-
               subset(subset.SHE.data, electrolyte == unique(subset.SHE.data$electrolyte))
         } else {
            subset.SHE.data <-
               subset(subset.SHE.data, electrolyte == electrolyte)
         }
      }

      # temperature
      # either happens to match a temperature in the dataset, or we interpolate
      # (under the assumption that potential varies linearly with temperature)
      if (!any(subset.SHE.data$temp == df$temperature[p])) {
         # sought temperature was not available in dataset, check that it falls inside
         # note: important to use less/more-than-or-equal in case data only contains one value
         if ((df$temperature[p] <= max(subset.SHE.data$temp)) &&
             (df$temperature[p] >= min(subset.SHE.data$temp))) {
            # within dataset range, do linear interpolation
            lm.subset <- stats::lm(SHE ~ temp, data = subset.SHE.data)
            # interpolated temperature, calculated based on linear regression
            # (more accurate than simple linear interpolation with approx())
            pot.interp <-
               lm.subset$coefficients[2] * df$temperature[p] + lm.subset$coefficients[1]
            ### CALC POTENTIAL vs SHE
            df$SHE[p] <-
               ifelse(df$vacuum[p],
                      pot.interp - df$potential[p],
                      pot.interp + df$potential[p])
         }
      } else {
         # requested temperature does exist in dataset
          ### CALC POTENTIAL vs SHE
         df$SHE[p] <-
            ifelse(df$vacuum[p],
                   subset(subset.SHE.data, temp == df$temperature[p])$SHE - df$potential[p],
                   subset(subset.SHE.data, temp == df$temperature[p])$SHE + df$potential[p])
      }
   }

   return(df$SHE)
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
   .Deprecated("as.SHE")
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
   .Deprecated("as.SHE")
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
