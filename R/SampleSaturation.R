#' Sample Saturation Identification Tool
#'
#' This function aims to identify the number of samples required per class in order to
#' get the highest accuracy results from a supervised classicifation.
#'
#' @param img A raster file.
#'
#' @param model The model which will be used for the classification. See \code{\link[caret]{train}} for model selection.
#'
#' @param trainData SpatialPolygonsDataFrame or SpatialPointsDataFrame containing the training locations.
#'
#' @param valData SpatialPolygonsDataFrame or SpatialPointsDataFrame containing the validation locations (optional). If no valData is given, the trainData will be split into 70 percent training and 30 percent validation.
#'
#' @param prodAcc TRUE or FALSE. If prodAcc is TRUE, the producer accuracy will be returned. If prodAcc is FALSE, the user accuracy will be returned.
#'
#' @param responseCol Character or integer giving the column in \code{trainData}, which contains the response variable.
#'
#' @param nSamples A vector containing the number of samples which will be used on the \code{\link[RStoolbox]{superClass}} function.
#'
#' @param overall TRUE or FALSE. Defines, whether the overall accuracy should be included or not.
#'
#' @param plot_graph TRUE or FALSE. Defines, whether the resulting data.frame should be automatically plotted or not.
#'
#' @return A data.frame with the accuracy numbers per class depending on the sample number.
#'
#' @examples
#' library(sp)
#' library(raster)
#' library(RStoolbox)
#' library(reshape2)
#' library(ggplot2)
#' library(randomForest)
#'
#' # Load sample raster file
#' my_raster <- raster::brick(system.file(package = "superClassAnalysis", "extdata", "landsat_sample.tif"))
#'
#' # Load sample training and validation data
#' my_train <- raster::shapefile(system.file(package = "superClassAnalysis", "extdata", "training_sample.shp"))
#' my_val <- raster::shapefile(system.file(package = "superClassAnalysis", "extdata", "validation_sample.shp"))
#'
#' # Execute SampleSaturation function
#' x = SampleSaturation(img = my_raster, model = 'rf', trainData = my_train,
#'                      valData = my_val, nSamples = c(10, 50, 100, 500),
#'                      responseCol = "class_name", prodAcc = TRUE,
#'                      overall = TRUE, plot_graph = TRUE)
#'
#' @export
SampleSaturation <- function(img, model, trainData, valData, prodAcc,
                             responseCol, nSamples, overall, plot_graph) {
  # Get class names of training data
  my_class_names <- names(trainData)
  # Get position of desired column within the names
  col_position <- match(responseCol, my_class_names)
  # Subset poly to the desired column
  trainData_subset <- trainData[,col_position]
  # Turn data.frame into a vector
  my_classes <- as.vector(trainData_subset@data[,1])
  # Calculate number of classes
  number_of_classes <- length(unique(my_classes))
  # check number of classes, if 2 make one class -> because only one positive class after classification
  if (number_of_classes == 2) {
    number_of_classes <- 1
  }
  # set number of columns depending on overall accuracy
  if (overall == TRUE) {
    colnumber <- 2 + number_of_classes
  } else {
    colnumber <- 1 + number_of_classes
  }
  # create data frame
  mat <- matrix(data = NA, nrow = length(nSamples), ncol = colnumber)
  acc.data <- as.data.frame(mat)
  # start for-loop for the number of different samples
  for (i in 1:length(nSamples)) {
    # Beginn Classification with or without validation data (30% used for validation)
    if (missing(valData)) {
      set.seed(7)
      x <- RStoolbox::superClass(img = img, trainData = trainData, trainPartition = 0.7,
                                 responseCol = responseCol, nSamples = nSamples[i],
                                 areaWeightedSampling = TRUE, model = model,
                                 mode = "classification", verbose = TRUE)
    } else {
      set.seed(7)
      x <- RStoolbox::superClass(img = img, trainData = trainData, valData = valData,
                                 responseCol = responseCol, nSamples = nSamples[i],
                                 areaWeightedSampling = TRUE, model = model,
                                 mode = "classification", verbose = TRUE)
    }
    # Fill table with number of samples
    acc.data[i,1] <- nSamples[i]
    # check numberof classes
    if (number_of_classes == 1) {
      # Check whether overall accuracy should be included or not
      # Check if user or producer accuracy should be used
      if (overall == TRUE) {
        acc.data[i,2] <- x$validation$performance$overall[1]
        # Check if user or producer accuracy should be used
        if (prodAcc == TRUE) {
          acc.data[i,3] <- x$validation$performance$byClass[1]
        } else {
          acc.data[i,3] <- x$validation$performance$byClass[3]
        }
      } else {
        if (prodAcc == TRUE) {
          acc.data[i,2] <- x$validation$performance$byClass[1]
        } else {
          acc.data[i,2] <- x$validation$performance$byClass[3]
        }
      }
    } else {
      if (overall == TRUE) {
        acc.data[i,2] <- x$validation$performance$overall[1]
        # Check if user or producer accuracy should be used
        if (prodAcc == TRUE) {
          for (z in 1:number_of_classes) {
            acc.data[i,2+z] <- x$validation$performance$byClass[z,1]
          }
        } else {
          for (z in 1:number_of_classes) {
            acc.data[i,2+z] <- x$validation$performance$byClass[z,3]
          }
        }
      } else {
        # Check if user or producer accuracy should be used
        if (prodAcc == TRUE) {
          for (z in 1:number_of_classes) {
            acc.data[i,1+z] <- x$validation$performance$byClass[z,1]
          }
        } else {
          for (z in 1:number_of_classes) {
            acc.data[i,1+z] <- x$validation$performance$byClass[z,3]
          }
        }
      }
    }
  }
  if (number_of_classes ==1) {
    positive_class <- x$validation$performance$positive
    if (prodAcc == TRUE) {
      rownames <- paste("Producer's Accuracy of", positive_class, sep=" ")
    } else {
      rownames <- paste("User's Accuracy of", positive_class, sep=" ")
    }
  } else {
    if (prodAcc == TRUE) {
      rownames <- paste("Prod", row.names(x$validation$performance$byClass), sep=" ")
    } else {
      rownames <- paste("User", row.names(x$validation$performance$byClass), sep=" ")
    }
  }
  # check if overall accuracy is included
  if (overall == TRUE) {
    names(acc.data) <- c("Number_of_Samples", "Overall_Accuracy", rownames)
  } else {
    names(acc.data) <- c("Number_of_Samples", rownames)
  }
  if (plot_graph == TRUE) {
    # melt data frame and plot results
    acc.data_long <- reshape2::melt(acc.data, id="Number_of_Samples")
    print(ggplot2::ggplot(data=acc.data_long, aes(x=Number_of_Samples, y=value, colour=variable)) +
            geom_line(size=1.05)+
            labs(y = "Accuracy", x = "Number of Samples", colour = "Legend:\n") +
            theme(legend.position=c(1,0),
                  legend.justification=c(1,0)))
  }
  return(acc.data)
}
