AMCA (R-package)
================

[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.15721.svg)](http://dx.doi.org/10.5281/zenodo.15721)

Automatic Model Configuration Algorithm (AMCA)

This is a data mining procedure based on unsupervised machine learning
techniques to automatically configure hydrological conceptual rainfall-runoff models such as FUSE.

**To cite this software:**  
Vitolo C., Automatic Model Configuration Algorithm (AMCA, R-package), (2015), GitHub repository, https://github.com/cvitolo/amca, doi: http://dx.doi.org/10.5281/zenodo.15721

#### Installation
Install and load packages
```R
# Install dependent packages from CRAN and GitHub:
install.packages(c("devtools", "tiger", "qualV"))
library(devtools)
install_github("cvitolo/fuse")

# Install the amca package
install_github("cvitolo/amca")
```

### Rainfall-Runoff modelling using FUSE
As an example, we could combine 50 parameter sets and 4 model structures to generate 200 model simulations.

Sample 50 parameter sets for FUSE, using LHS method
```R
library(fuse)
data("fuse_hydrological_timeseries")

set.seed(123)    
parameters <- fuse::generateParameters(NumberOfRuns = 50)
```

Choose a list of models to take into account
```R
selectedModels <- c(60, 230, 342, 426) 
```

Run simulations
```R
library(amca)
amca::MCsimulations(DATA = fuse_hydrological_timeseries,
                    parameters = parameters,
                    deltim = 1/24,
                    warmup = 500,
                    ListOfModels = selectedModels)
```

### Find the best configuration(s) amongst those simulated
Run the AMCA algorithm:
```R
results <- amca(DATA = fuse_hydrological_timeseries,
                parameters = parameters,
                deltim = 1/24,
                warmup = 500,
                selectedModels = selectedModels)
```

The best configuration is stored in
```R
results$RE

PlotEnsembles(bounds = results$ts$bounds,
              dischargeTable = results$ts$discharges)
```

### Warnings
This package and functions herein are part of an experimental open-source project. They are provided as is, without any guarantee.

# Leave your feedback
I would greatly appreciate if you could leave your feedbacks via email (cvitolodev@gmail.com).
