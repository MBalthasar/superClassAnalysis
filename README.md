# superClassAnalysis
A toolbox for improving supervised classification accuracies

## Installation

To install the current version, use `devtools`.

```R
devtools::install_github("MBalthasar/superClassAnalysis")
```

## Available Functions

The following functions are currently available and tested on Windows 10.

* `LoadPackages()` This function loads a bunch of commonly used packages for spatial data manipulation. Alternatively the user can provide a vector containing the desired package names which should be installed if necessary and loaded.
* `SampleSaturation()` This function aims to identify the number of samples required per class in order to get the highest accuracy results from a supervised classicifation.
* `ResolutionSaturation()` This function aims to identify the best spatial resolution required per class in order to get the highest accuracy results from a supervised classicifation.
* `PolygonSaturation()` This function aims to identify the best number of polygons required per class within a training data set in order to get the highest accuracy results from a supervised classicifation. It furthermore enables the user to identify polygons which lead to lower classification accuracies for training data quality testing. The function will always use the same number of polygons per class and is therefore limited by the class with the least amount of polygons. It is overall recommended to have the same number of polygons for each class. he function will start with 2 polygons per class.
