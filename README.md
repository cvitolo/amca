AMCA (R-package)
================

Automatic Model Configuration Algorithm (AMCA)

This is a data mining procedure based on unsupervised machine learning
techniques to automatically configure hydrological conceptual rainfall-runoff models such as FUSE.

#### Basics
Install and load packages
```R
# Install dependent packages from CRAN:
x <- c("qualV", "ggplot2", "emoa", "som", "dtw", "tgp", "devtools",
       "reshape2", "colorspace", "RColorBrewer","zoo","tiger")
install.packages(x)
lapply(x, require, character.only=T); rm(x)

# Install dependent package from R-Forge:
# install.packages("fuse", repos="http://R-Forge.R-project.org")

# Install dependent gists and packages from github:
library(devtools)
install_github("cvitolo/r_pure", subdir = "pure")
install_github("cvitolo/r_amca", subdir = "amca")
```

### Rainfall-Runoff modelling using FUSE
As an example, we could combine 50 parameter sets and 4 model structures to generate 200 model simulations.

Sample 50 parameter sets for FUSE, using LHS method
```R
library(fuse)
data(DATA)

outputFolder <- "~"
deltim <- 1/24 
warmup <- round(dim(DATA)[1]/10,0)

NumberOfRuns <- 10
set.seed(123)    
parameters <- GeneratePsetsFUSE(NumberOfRuns)
```

Choose a list of models to take into account
```R
selectedModels <- c(60,230,342,426) 
```

Define the list of Model Performance Indices (MPIs)
```R
library(tiger)
library(qualV)
LAGTIME = function(x) lagtime(x$Qo,x$Qs)    
MAE     = function(x) mean(x$Qs - x$Qo, na.rm = TRUE)            
NSHF    = function(x) 1 - EF(x$Qo,x$Qs)           
NSLF    = function(x) 1 - EF( log(x$Qo) , log(x$Qs) )           
RR      = function(x) sum(x$Qs) /sum(x$Po)

MPIs <- list("LAGTIME"=LAGTIME,"MAE"=MAE,"NSHF"=NSHF,"NSLF"=NSLF,"RR"=RR)
```

Run simulations
```R
# It is recommended to run simulations on HPC facilities. 
# However small batches can be run locally using the function MCsimulations()
library(pure)
MCsimulations(DATA,deltim,warmup,parameters,selectedModels,outputFolder,MPIs)
```

### Find the best configuration(s) amongst those simulated
Run the AMCA algorithm:
```R
library(amca)
results <- amca(DATA, parameters, MPIs, outputFolder, deltim,
                selectedModels=selectedModels, warmup, verbose=FALSE,
                PreSel=TRUE,allBounds=FALSE)
```

The best configuration is stored in
```R
results$RETable

PlotEnsembles(results$BoundsRE$bounds, results$BoundsRE$discharges,
              label1="min-max", label2="percentiles")
```

### Warnings
This package and functions herein are part of an experimental open-source project. They are provided as is, without any guarantee.

# Leave your feedback
I would greatly appreciate if you could leave your feedbacks via email (cvitolodev@gmail.com).
