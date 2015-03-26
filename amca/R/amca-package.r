#' amca.
#'
#' @name Automatic Model Configuration Algorithm (AMCA)
#' @docType package
#'
#' @description Data mining algorithm based on unsupervised machine learning techniques to automatically configure hydrological conceptual rainfall-runoff models.
#'
#' @section Description:
#' Hydrological modelling practice is characterised by a large degree of subjectivity, especially in the selection of model structures and parameter ranges. When making these decisions, a wide range of options exists and a particular choice is often hard to justify on the basis of model assumptions and knowledge about the hydrological system that is to be modelled. This package implements a data mining approach, based on machine learning techniques, to make the definition of parameter ranges and selection of model structures more transparent. An algorithm is presented to facilitate an explicit approach to model structure selection, parameter identifiability and redundancy reduction. Basic statistics and set theory operations are combined with Pareto filtering, clustering techniques and time series matching algorithms in a multi-objective calibration framework. The result of the algorithm is a set of ``suggested model configurations'', that tend to be less modeller dependent, and more consistent and reproducible than traditional approaches.
#'
#' @references
#' Vitolo C, Buytaert W, 2013, Data Mining of Hydrological Model Performance, EGU General Assembly 2013, Publisher: European Geosciences Union.
#'
NULL
