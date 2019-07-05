#' Function for Loading a Bunch of Packages
#'
#' This function loads a bunch of commonly used packages for spatial data manipulation.
#' Alternatively the user can provide a vector containing the desired package names which should
#' be installed if necessary and loaded
#'
#' @param package_vector A vector containing package names as character strings.
#' If no vector is provided a bunch of commonly used packages for spatial data manipulation are loaded.
#' Namely: "sp", "raster", "maptools", "rgdal", "rgeos", "ggmap", "snow", "randomForest", "RStoolbox",
#' "e1071", "kernlab", "mda", "pls", "caret", "ggplot2", "ggsn", "maps", "plyr", "reshape2", "caret".
#'
#' @examples
#' # Create vector with package names
#' x <- c("raster", "sp", "ggplot2")
#'
#' # install if necessary and load packages
#' LoadPackages(package_vector = x)
#'
#' # Load a bunch of commonly used packages for spatial data manipulation
#' LoadPackages()
#'
#' @export
LoadPackages <- function(package_vector) {
  if (missing(package_vector)) {
    package_vector <- c("sp", "raster", "maptools", "rgdal", "rgeos", "ggmap",
                        "snow", "randomForest", "RStoolbox", "e1071", "kernlab",
                        "mda", "pls", "caret", "ggplot2", "ggsn", "maps",
                        "plyr", "reshape2", "caret")
  }
  for (i in 1:length(package_vector)) {
    if (!require(package_vector[i], character.only = TRUE)) {
      install.packages(package_vector[i])
    }
    library(package_vector[i], character.only = TRUE)
  }
}
