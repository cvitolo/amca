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
       "reshape2", "colorspace", "RColorBrewer")
install.packages(x)
lapply(x, require, character.only=T); rm(x)

# Install dpendent package from R-Forge:
# install.packages("fuse", repos="http://R-Forge.R-project.org")
library(fuse)

# Install dependent gists and packages from github:
library(devtools)
install_github("r_pure", username = "cvitolo", subdir = "pure")
library(pure)
install_github("r_amca", username = "cvitolo", subdir = "amca")
library(amca)
```

### Rainfall-Runoff modelling using FUSE
As an example, we could combine 50 parameter sets and 4 model structures to generate 200 model simulations.

Sample 50 parameter sets for FUSE, using LHS method
```R
library(fuse)
data(DATA)

set.seed(123)
NumberOfRuns <- 10    
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
library(pure)
outputFolder <- "~"
deltim <- 1/24 # or multiplier/60/60/24
warmup <- round(dim(DATA)[1]/10,0)

# It is recommended to run simulations on HPC facilities. 
# However small batches can be run locally using the function MCsimulations()
MCsimulations(DATA,deltim,warmup,parameters,ModelList,outputFolder,MPIs)
```

### Find the best configuration(s) amongst those simulated
Run the AMCA algorithm:
```R
library(amca)
results <- amca(DATA, parameters, MPIs, outputFolder, selectedModels, warmup)
```

The best configuration is stored in
```R
results$RETable

PlotEnsembles(results$BoundsRE$bounds, results$BoundsRE$discharges,
              label1="min-max", label2="percentiles")
```
