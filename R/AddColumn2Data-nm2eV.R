#' AddColumnEnergy
#'
#' A poorly documented function.
#' To be honest I don't remember what it's for. Not exported.
#'
#' @param path.to.folder full path to folder containing data files
#'
#' @return Writes to file.
AddColumnEnergy <- function(path.to.folder) {
  txt.files <-
     list.files(path = path.to.folder, pattern = "\\.txt$",
                full.names = TRUE)
  # message(txt.files)
  for (f in 1:length(txt.files)) {
     this.file <- txt.files[f]
     # read the file contents into df
     this.data <-
        utils::read.table(this.file, header = T,
                   col.names = c("wavelength", "intensity"))
     # add energy (eV) as third column
     this.data$energy <- photoec::wavelength2energy(this.data$wavelength)
     # message(head(this.data))
     utils::write.table(this.data,
                 file = this.file,
                 sep = "\t",
                 col.names = c("#Wave", "#Intensity", "#Energy"),
                 row.names = FALSE)
  }
}
