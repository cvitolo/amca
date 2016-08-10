################################################################################
# SCRIPT TO RUN EXPERIMENTS IN PAPER AMCA ON THE WRR JOURNAL                   #
# Author: Claudia Vitolo                                                       #
# Date: 2016                                                                   #
################################################################################

# INSTALL AND LOAD AMCA LIBRARY AND DEPENDENCIES
dependentPackages <- c("zoo", "xts", "ggplot2", "som", "dtw", "reshape2",
                       "fanplot", "colorspace", "RColorBrewer", "emoa",
                       "qualV", "tiger", "scales", "tidyr", "devtools")

# install missing packages
missingPackages <- which(!(dependentPackages %in% rownames(installed.packages())))

if (length(missingPackages) > 0) {
  install.packages(dependentPackages[missingPackages])
}

library(devtools)

rm(list=ls(all=TRUE))

################################################################################
# SET WORKING DIRECTORY ########################################################
################################################################################

setwd("/media/claudia/Maxtor/amca/MOPEX/")

################################################################################
# PREPARE OBSERVED DATA ########################################################
################################################################################

# install_github("cvitolo/r_rnrfa", subdir = "rnrfa")
# install_github("cvitolo/r_hddtools", subdir = "hddtools")
library(hddtools)

# MOPEX full catalogue
x <- catalogueMOPEX()

# Extract time series for the 12 catchments in Duan et al. 2006
MOPEX12 <- c("01608500", "01643000", "01668000", "03054500", "03179000",
             "03364000", "03451500", "05455500", "07186000", "07378500",
             "08167500", "08172000")
x <- x[which(x$id %in% MOPEX12),]

# For example:
DATA <- tsMOPEX("03451500")                                             # mm/day
# Remove any trailing -99 (NAs) in Q
if ( any(DATA[,"Q"] == -99) ) {
  DATA <- DATA[1:(which(DATA[,"Q"] == -99)[1] - 1), ]
}

idx <- round(dim(DATA)[1]/2,0)

training <- DATA[1:idx, ]
testing <- DATA[(idx+1):dim(DATA)[1], ]

# check visually
plot(y = DATA$Q, x = 1:dim(DATA)[1], xlab="", type="l")
lines(y = training$Q, x = 1:dim(training)[1], col="red")
lines(y = testing$Q, x = (idx+1):(idx+dim(testing)[1]), col="red")

################################################################################
# GENERATE SYNTHETHIC EXAMPLE (using the French Broad River)  ##################
################################################################################

# install_github("cvitolo/r_fuse", subdir = "fuse")
# install_github("cvitolo/r_amca", subdir = "amca")

# LOAD LIBRARIES
library(fuse)
library(amca)

ModelList <- PrepareTable()

# sample a model structure
set.seed(012)
TRUEmodel <- sample(ModelList$mid, 1)
# To know what options and parameters are available for a given model
x <- fuseInfo(TRUEmodel)

TRUEparams <- generateParameters(NumberOfRuns = 1,
                                 SamplingType = "LHS",
                                 params2remove = names(x)[which(x==FALSE)])
# saveRDS(object = TRUEparams, file = "TRUEparams.rds")

# Info on the DATA period
# paste("from", index(DATA)[1], "to", index(DATA)[dim(DATA)[1]])

Q <- fuse(DATA = DATA[,c("P", "E")],
          ParameterSet = TRUEparams,
          deltim = 1,
          mid = TRUEmodel)
# Check how different synthetic and observed Q are
# plot(Q, type="l"); lines(DATA$Q, col="red")
# Substitute synthetic Q to observed Q
DATA <- cbind(DATA[,c("P", "E")], Q)
saveRDS(object = DATA,
        file = "DATASynthetic_03451500.rds")

rm(list=ls(all=TRUE))

################################################################################
# AMCA STEP BY STEP ############################################################
################################################################################

setwd("/media/claudia/Maxtor/amca/MOPEX/")

library(fuse)
library(amca)

ObsIndicesNames <- c("LAGTIME","MAE","NSHF","NSLF")
ResultsFolder <- "CATCHMENT_03451500_synthetic/"
numberOfParamSets <- 10000
# fraction to use for warmup
fractionWarmUp <- 0.50
# Define the time step (1 day for all the MOPEX data)
deltim <- 1

TRUEparams <- readRDS("TRUEparams.rds")
DATA <- readRDS("DATASynthetic_03451500.rds")
warmup <- round(dim(DATA)[1]/2,0)
pperiod <- (warmup + 1):dim(DATA)[1]
# Generate parameters for simulations
set.seed(0123)
parameters <- generateParameters(NumberOfRuns = 10000,
                                 SamplingType = "LHS")

ModelList <- PrepareTable()[seq(1,1248,2),]

# RUN FUSE
# Option A (recommended): on HPC using FUSExMOPEX12.sh/FUSExMOPEX12synthetic.sh
# Option B: run simulations locally using the following command
#           MCsimulations(DATA = DATA, deltim = 1, warmup = warmup,
#                         parameters = parameters, ListOfModels = ModelList$mid,
#                         SimulationFolder = ResultsFolder,
#                         MPIs = ObsIndicesNames, verbose = TRUE)

# Check if all the result files have been calculated:
ScanResultFiles(ResultsFolder, ModelList)

# For the synthetic case, don't run amca() but run the algorithm step by step

### Build Initial Ensemble #####################################################
# Use only MPIs
IndicesRaw <- Simulations2Indices(ModelList, ResultsFolder, numberOfParamSets,
                                  nIndices=length(ObsIndicesNames),
                                  verbose=TRUE, string = "indicesTraining")
# Check NAs
any(is.na(IndicesRaw[,1,]))
any(is.na(IndicesRaw[,2,]))
any(is.na(IndicesRaw[,3,]))
any(is.na(IndicesRaw[,4,]))
# This is probably due to negative simulated Q!
IndicesRaw[is.na(IndicesRaw)] <- -Inf

# The ObsIndices should all be 0. There is no need to substract the true
# (observed) indices from the raw one + absolute value.
# Just calculate the absolute value, then rescale between 0 and 1.
Indices <- RescaleIndices(IndicesRaw)
# saveRDS(Indices, "Indices.rds") # This is array P
# Indices <- readRDS("Indices.rds")
# rm(IndicesRaw)

# Generate plots
library(reshape2);library(ggplot2)
set.seed(123)
pidSample <- sample(x = 1:dim(Indices)[1], size = 1000)
Plot2DarrayMultiple(Indices[pidSample,,], ModelList)

library(manipulate)
manipulate(plot(apply(Indices[,x,],1,median, na.rm = TRUE),
                type="l",
                main = paste("Median", ObsIndicesNames[x]),
                xlab = "param position", ylab = ""),
           x = slider(1, 4))
manipulate(plot(apply(Indices[,x,],2,median, na.rm = TRUE),
                type="l",
                main = paste("Median", ObsIndicesNames[x]),
                xlab = "model position", ylab = ""),
           x = slider(1, 4))

# Every model has 10000 realisation and we can calculate the median of them to have a score of how well the model performs, regardless of parameters.
# Then, to compare models, we can calculate the median of models' medians and use it as threshold to define whether a model is performing generally well or not, compared to others.
# The same reasoning can be applied to parameter sets.

library(MASS)       # load the MASS package
# LAGTIME: first half performs worse than second half of models
#          (red horizontal strips)
# Models' median of medians, this threshold seems to work as cut-off point
threshold <- median(apply(Indices[,1,], 2, median, na.rm = TRUE))
midcounter <- which(apply(Indices[,1,], 2, median, na.rm = TRUE) > threshold)
performance <- ifelse(ModelList$mid %in% ModelList$mid[midcounter], "Non-Behavioural", "Behavioural")
# H0: Is the LAGTIME indipendent of the model building decisions?
# Let's look at contingency tables and the Chi-squared test of independence
ModelList2 <- ModelList
ModelList2$arch1[ModelList$arch1 == 21] <- "onestate_1"
ModelList2$arch1[ModelList$arch1 == 22] <- "tension1_1"
ModelList2$arch1[ModelList$arch1 == 23] <- "tension2_1"
chisq.test(table(performance, ModelList2$arch1))$p.value   # Yes, p-value > 0.05
ModelList2$arch2[ModelList$arch2 == 31] <- "fixedsiz_2"
ModelList2$arch2[ModelList$arch2 == 32] <- "tens2pll_2"
ModelList2$arch2[ModelList$arch2 == 33] <- "unlimfrc_2"
ModelList2$arch2[ModelList$arch2 == 34] <- "unlimpow_2"
chisq.test(table(performance, ModelList2$arch2))$p.value
ModelList2$qsurf[ModelList$qsurf == 41] <- "arno_x_vic"
ModelList2$qsurf[ModelList$qsurf == 42] <- "prms_varnt"
ModelList2$qsurf[ModelList$qsurf == 43] <- "tmdl_param"
chisq.test(table(performance, ModelList2$qsurf))$p.value
ModelList2$qperc[ModelList$qperc == 51] <- "perc_f2sat"
ModelList2$qperc[ModelList$qperc == 52] <- "perc_lower"
ModelList2$qperc[ModelList$qperc == 53] <- "perc_w2sat"
chisq.test(table(performance, ModelList2$qperc))$p.value    # Yes, p-value > 0.05
ModelList2$esoil[ModelList$esoil == 61] <- "rootweight"
ModelList2$esoil[ModelList$esoil == 62] <- "sequential"
chisq.test(table(performance, ModelList2$esoil))$p.value    # Yes, p-value > 0.05
ModelList2$qintf[ModelList$qintf == 71] <- "intflwnone"
ModelList2$qintf[ModelList$qintf == 72] <- "intflwsome"
chisq.test(table(performance, ModelList2$qintf))$p.value
ModelList2$q_tdh[ModelList$q_tdh == 81] <- "no_routing"
ModelList2$q_tdh[ModelList$q_tdh == 82] <- "rout_gamma"
chisq.test(table(performance, ModelList2$q_tdh))$p.value
# Parameters' median of medians
threshold <- median(apply(Indices[,1,], 1, median, na.rm = TRUE))
pidcounter <- which(apply(Indices[,1,1:312], 1, median, na.rm = TRUE) >= threshold)
performance <- ifelse(1:10000 %in% pidcounter, "Non-Behavioural", "Behavioural")
timedelay <- cut(parameters$timedelay, breaks = c(0,1,2,3,4,5))
chisq.test(table(performance, timedelay))$p.value

RealisationsTable <- data.frame("mid" = rep(ModelList$mid[midcounter],
                                            each=length(pidcounter)),
                                "pid" = rep(pidcounter,
                                            times=length(ModelList$mid[midcounter])))
tableX <- ExtendTable(RealisationsTable, ModelList, Indices, parameters,
                      ObsIndicesNames)
PlotParameterSimilarities(tableX,
                          parameters,
                          selectedParams=NULL,
                          synparameters=TRUEparams)

# NSLF: red vertical strips
# Models' median of medians
threshold <- median(apply(Indices[,4,], 2, median, na.rm = TRUE))
midcounter <- which(apply(Indices[,4,], 2, median, na.rm = TRUE) > threshold)
performance <- ifelse(ModelList$mid %in% ModelList$mid[midcounter], "Non-Behavioural", "Behavioural")
# H0: Is the LAGTIME indipendent of the model building decisions?
# Let's look at contingency tables and the Chi-squared test of independence
chisq.test(table(performance, ModelList2$arch1))$p.value
chisq.test(table(performance, ModelList2$arch2))$p.value
chisq.test(table(performance, ModelList2$qsurf))$p.value
chisq.test(table(performance, ModelList2$qperc))$p.value
chisq.test(table(performance, ModelList2$esoil))$p.value
chisq.test(table(performance, ModelList2$qintf))$p.value
chisq.test(table(performance, ModelList2$q_tdh))$p.value
# Parameters' median of medians
threshold <- median(apply(Indices[,4,], 1, median, na.rm = TRUE))
pidcounter <- 312 + which(apply(Indices[,4,313:624], 1, median, na.rm = TRUE) >= threshold)
RealisationsTable <- data.frame("mid" = rep(ModelList$mid[midcounter],
                                            each=length(pidcounter)),
                                "pid" = rep(pidcounter,
                                            times=length(ModelList$mid[midcounter])))
tableX <- ExtendTable(RealisationsTable, ModelList, Indices, parameters,
                      ObsIndicesNames, verbose = TRUE)
PlotParameterSimilarities(tableX,
                          parameters,
                          selectedParams=NULL,
                          synparameters=TRUEparams)
PlotModelSimilarities(ModelList, tableX,
                      synMID = TRUEmodel, plotType="frequency",
                      myTitle="", modellingOptions=c("arch1","arch2",
                                                     "qsurf","qperc",
                                                     "esoil","qintf","q_tdh"))

# NSHF: red vertical/horizontal strips
# Models' median of medians
threshold <- median(apply(Indices[,3,], 2, median, na.rm = TRUE))
midcounter <- which(apply(Indices[,3,], 2, median, na.rm = TRUE) > threshold)
performance <- ifelse(ModelList$mid %in% ModelList$mid[midcounter], "Non-Behavioural", "Behavioural")
# H0: Is the NSHF indipendent of the model building decisions?
# Let's look at contingency tables and the Chi-squared test of independence
chisq.test(table(performance, ModelList2$arch1))$p.value
chisq.test(table(performance, ModelList2$arch2))$p.value
chisq.test(table(performance, ModelList2$qsurf))$p.value
chisq.test(table(performance, ModelList2$qperc))$p.value
chisq.test(table(performance, ModelList2$esoil))$p.value
chisq.test(table(performance, ModelList2$qintf))$p.value
chisq.test(table(performance, ModelList2$q_tdh))$p.value
# Parameters' median of medians
threshold <- median(apply(Indices[,3,], 1, median, na.rm = TRUE))
pidcounter <- which(apply(Indices[,3,], 1, median, na.rm = TRUE) >= threshold)
performance <- ifelse(1:10000 %in% pidcounter, "Non-Behavioural", "Behavioural")
timedelay <- cut(parameters$timedelay, breaks = c(0,1,2,3,4,5))
chisq.test(table(performance, timedelay))$p.value

RealisationsTable <- data.frame("mid" = rep(ModelList$mid[midcounter],
                                            each=length(pidcounter)),
                                "pid" = rep(pidcounter,
                                            times=length(ModelList$mid[midcounter])))
tableX <- ExtendTable(RealisationsTable, ModelList, Indices, parameters,
                      ObsIndicesNames)
PlotParameterSimilarities(tableX,
                          parameters,
                          selectedParams=NULL,
                          synparameters=TRUEparams)
PlotModelSimilarities(ModelList, tableX,
                      synMID = TRUEmodel, plotType="frequency",
                      myTitle="", modellingOptions=c("arch1","arch2",
                                                     "qsurf","qperc",
                                                     "esoil","qintf","q_tdh"))

# MAE: red vertical/horizontal strips
# Models' median of medians
threshold <- median(apply(Indices[,2,], 2, median, na.rm = TRUE))
midcounter <- which(apply(Indices[,2,], 2, median, na.rm = TRUE) > threshold)
performance <- ifelse(ModelList$mid %in% ModelList$mid[midcounter], "Non-Behavioural", "Behavioural")
# H0: Is the NSHF indipendent of the model building decisions?
# Let's look at contingency tables and the Chi-squared test of independence
#library(xtable) xtable(table(performance, ModelList2$arch1))
chisq.test(table(performance, ModelList2$arch1))$p.value
chisq.test(table(performance, ModelList2$arch2))$p.value
chisq.test(table(performance, ModelList2$qsurf))$p.value
chisq.test(table(performance, ModelList2$qperc))$p.value
chisq.test(table(performance, ModelList2$esoil))$p.value
chisq.test(table(performance, ModelList2$qintf))$p.value
chisq.test(table(performance, ModelList2$q_tdh))$p.value
# Parameters' median of medians
threshold <- median(apply(Indices[,3,], 1, median, na.rm = TRUE))
pidcounter <- which(apply(Indices[,3,], 1, median, na.rm = TRUE) >= threshold)
performance <- ifelse(1:10000 %in% pidcounter, "Non-Behavioural", "Behavioural")
timedelay <- cut(parameters$timedelay, breaks = c(0,1,2,3,4,5))
chisq.test(table(performance, timedelay))$p.value

RealisationsTable <- data.frame("mid" = rep(ModelList$mid[midcounter],
                                            each=length(pidcounter)),
                                "pid" = rep(pidcounter,
                                            times=length(ModelList$mid[midcounter])))
head(RealisationsTable)
tableX <- ExtendTable(RealisationsTable, ModelList, Indices, parameters,
                      ObsIndicesNames)
PlotParameterSimilarities(tableX,
                          parameters,
                          selectedParams=NULL,
                          synparameters=TRUEparams)
PlotModelSimilarities(ModelList, tableX,
                      synMID = TRUEmodel, plotType="frequency",
                      myTitle="", modellingOptions=c("arch1","arch2",
                                                     "qsurf","qperc",
                                                     "esoil","qintf","q_tdh"))

# Check NAs
temp <- which(is.na(Indices), arr.ind = TRUE)
RealisationsTable <- data.frame("mid" = temp[,3], "pid" = temp[,1])
RealisationsTable[,1] <- ModelList$mid[RealisationsTable[,1]]
head(RealisationsTable)
tableX <- ExtendTable(RealisationsTable, ModelList, Indices, parameters,
                      ObsIndicesNames)
tableNA1 <- tableX[is.na(tableX$LAGTIME),]
tableNA2 <- tableX[is.na(tableX$MAE),]
tableNA3 <- tableX[is.na(tableX$NSHF),]
tableNA4 <- tableX[is.na(tableX$NSLF),]
PlotParameterSimilarities(tableNA4,
                          parameters,
                          selectedParams=NULL,
                          synparameters=TRUEparams)
PlotModelSimilarities(ModelList, tableNA4,
                      synMID = TRUEmodel, plotType="frequency",
                      myTitle="", modellingOptions=c("arch1","arch2",
                                                     "qsurf","qperc",
                                                     "esoil","qintf","q_tdh"))

plot(ecdf(na.omit(Indices[,1,])), main = "ECDF", xlab = "")
lines(ecdf(na.omit(Indices[,2,])), col = 2)
lines(ecdf(na.omit(Indices[,3,])), col = 3)
lines(ecdf(na.omit(Indices[,4,])), col = 4)
legend("bottomright", legend = ObsIndicesNames, col=1:4, lty = 1)

# Make a table with models and params identification numbers
ALLrealisations <- data.frame("mid" = rep(ModelList$mid,
                                          each = numberOfParamSets),
                              "pid" = rep(1:numberOfParamSets,
                                          times = length(ModelList$mid)))

IEtable <- ExtendTable(realisations = ALLrealisations,
                       ModelList = ModelList,
                       Indices = Indices, parameters = parameters,
                       ObsIndicesNames = ObsIndicesNames,
                       verbose = TRUE, onlyIndices = TRUE)
# saveRDS(IEtable, "IEtable.rds")
# IEtable <- readRDS("IEtable.rds")

# Now start using discharges
ResultsFolder <- "CATCHMENT_03451500_synthetic_discharges/"

# Build Initial Ensemble envelop using simulated discharges
IE <- BuildEnsemble(observedQ = DATA$Q[pperiod],
                    SimulationFolder = ResultsFolder,
                    MIDs = ModelList$mid, PIDs = 1:dim(parameters)[1],
                    bigfile = TRUE)
# saveRDS(IE, "IE.rds")
# library(dygraphs)
# dygraph(IE[,c("Dates", "Qobs", "lQ", "uQ")]) %>% dyRangeSelector()

library(som)
# TEST: Cluster everything
quantizationError <- c()

for (dimXY in c(5, 10, 15, 20, 25)){

  print(paste("training SOM with dimensions:", dimXY, "x", dimXY))

  the.som <- som(IEtable[,ObsIndicesNames],
                 xdim=dimXY, ydim=dimXY,
                 init="linear",neigh="gaussian",topol="rect")

  quantizationError <- c(quantizationError, qerror(the.som))
}

plot(quantizationError)
# Choose dimension that minimises the quantization error in the som
somDim <- c(5, 10, 15, 20, 25)[which.min(quantizationError)]

the.som <- som(IEtable[,ObsIndicesNames],
               xdim=somDim, ydim=somDim,
               init="linear",neigh="gaussian",topol="rect")

IEtable$ClusterX <- the.som$visual[,1]
IEtable$ClusterY <- the.som$visual[,2]

head(IEtable)

################################################################################
# STEP E: PARETO FRONTIER ######################################################
################################################################################

PF <- ParetoFrontier(IEtable, ObsIndicesNames)
# saveRDS(PF, "PF.rds")
# unique models:
length(unique(PF$mid))
# unique p. sets:
length(unique(PF$pid))

PFext <- ExtendTable(realisations = PF[,c("mid","pid")], ModelList, Indices,
                     parameters, ObsIndicesNames)
# interquartile range of timedelay in PF:
summary(PFext$timedelay, na.rm = TRUE)[c(2,5)]

# In terms of timedelay, is the separation between Behavioural and Non-Behavioural significant?
performance <- ifelse(1:10000 %in% unique(PF$pid), "Behavioural", "Non-Behavioural")
timedelay <- cut(parameters$timedelay, breaks = c(0,1,2,3,4,5))
chisq.test(table(performance, timedelay))$p.value # YES!

# In terms of routing, is the separation between Behavioural and Non-Behavioural significant?
performance <- ifelse(ModelList$mid %in% unique(PF$mid), "Behavioural", "Non-Behavioural")
table(performance, ModelList$q_tdh)
chisq.test(table(performance, ModelList$q_tdh))$p.value # YES!

# In terms of evaporation, is the separation between Behavioural and Non-Behavioural significant?
table(performance, ModelList$esoil)
chisq.test(table(performance, ModelList$esoil))$p.value # YES!

chisq.test(table(performance, ModelList$arch2))$p.value # YES!

PlotParameterSimilarities(PFext,
                          parameters,
                          selectedParams=NULL,
                          synparameters=TRUEparams)
PlotModelSimilarities(ModelList, PFext,
                      synMID = TRUEmodel, plotType="frequency",
                      myTitle="", modellingOptions=c("arch1","arch2",
                                                     "qsurf","qperc",
                                                     "esoil","qintf","q_tdh"))
#library(xtable) xtable(table(performance, ModelList2$arch1))
chisq.test(table(performance, ModelList2$arch1))$p.value
chisq.test(table(performance, ModelList2$arch2))$p.value
chisq.test(table(performance, ModelList2$qsurf))$p.value
chisq.test(table(performance, ModelList2$qperc))$p.value
chisq.test(table(performance, ModelList2$esoil))$p.value
chisq.test(table(performance, ModelList2$qintf))$p.value
chisq.test(table(performance, ModelList2$q_tdh))$p.value

# T2 <- BuildEnsemble(observedQ = DATA$Q[pperiod],
#                     SimulationFolder = ResultsFolder,
#                     realisations = PF, verbose = TRUE, outputQ = TRUE)
# saveRDS(T2, "T2.rds")
#
# IE <- readRDS("IE.rds")
# T1 <- readRDS("T1.rds")
# temp <- review(IE, T1, rel= FALSE)
# temp <- review(IE, T2, rel= FALSE)
#
# library(dygraphs)
# library(xts)
# df <- xts(data.frame("Qobs" = IE$Qobs,
#                      "T1lQ" = T1$bounds$lQ,
#                      "T1mQ" = T1$bounds$mQ,
#                      "T1uQ" = T1$bounds$uQ,
#                      "T2lQ" = T2$bounds$lQ,
#                      "T2mQ" = T2$bounds$mQ,
#                      "T2uQ" = T2$bounds$uQ, stringsAsFactors = FALSE),
#           order.by = dates[pperiod])
# saveRDS(df, "dfT1T2.rds")
#
# str(index(df))
# which(index(df)=="1975-01-31")
#
# dygraph(df[4894:4900,], main = "Prediction Intervals (90%)") %>%
#   dyAxis("x", drawGrid = FALSE) %>%
#   dySeries(c("T1lQ", "T1mQ", "T1uQ"), label = "T'") %>%
#   dySeries(c("T2lQ", "T2mQ", "T2uQ"), label = "T''") %>%
#   dySeries(c("Qobs"), label = "Qobs") %>%
#   dyOptions(colors = brewer.pal(8, "Dark2")[c(2,3,8)]) %>%
#   dyLegend(width = 500) %>%
#   dyRangeSelector()

################################################################################
# STEP F: REDUNDANCY REDUCTION #################################################
################################################################################

RE <- RedundancyReduction(PF, observedQ = DATA$Q[pperiod], parameters, deltim,
                          ResultsFolder, ObsIndicesNames, verbose=TRUE)
# saveRDS(RE, "RE.rds")
REext <- ExtendTable(RE, ModelList, Indices, parameters, ObsIndicesNames)
PlotParameterSimilarities(REext,
                          parameters,
                          selectedParams=NULL,
                          synparameters=TRUEparams)
PlotModelSimilarities(ModelList, REext,
                      synMID = TRUEmodel, plotType="frequency",
                      myTitle="", modellingOptions=c("arch1","arch2",
                                                     "qsurf","qperc",
                                                     "esoil","qintf","q_tdh"))

T3 <- BuildEnsemble(observedQ = DATA$Q[pperiod],
                    SimulationFolder = ResultsFolder,
                    realisations = RE, verbose = TRUE, outputQ = TRUE)
saveRDS(T3, "T3.rds")

temp <- review(IE, T3, rel= TRUE)

library(dygraphs)
library(xts)
df <- xts(data.frame("Qobs" = IE$Qobs,
                     "T2lQ" = T2$bounds$lQ,
                     "T2mQ" = T2$bounds$mQ,
                     "T2uQ" = T2$bounds$uQ,
                     "T3lQ" = T3$bounds$lQ,
                     "T3mQ" = T3$bounds$mQ,
                     "T3uQ" = T3$bounds$uQ, stringsAsFactors = FALSE),
          order.by = dates[pperiod])
saveRDS(df, "dfT2T3.rds")

dygraph(df[4894:4900,], main = "Prediction Intervals (90%)") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dySeries(c("T2lQ", "T2mQ", "T2uQ"), label = "T''") %>%
  dySeries(c("T3lQ", "T3mQ", "T3uQ"), label = "T'''") %>%
  dySeries(c("Qobs"), label = "Qobs") %>%
  dyOptions(colors = brewer.pal(8, "Dark2")[c(3,4,8)]) %>%
  dyLegend(width = 500) %>%
  dyRangeSelector()

# OR
df <- xts(data.frame("Qobs" = IE$Qobs,
                     "Tmin" = IE$minQ,
                     "Qobs2" = IE$Qobs,
                     "Tmax" = IE$maxQ,
                     "T3lQ" = T3$bounds$lQ,
                     "T3mQ" = T3$bounds$mQ,
                     "T3uQ" = T3$bounds$uQ, stringsAsFactors = FALSE),
          order.by = dates[pperiod])
saveRDS(df, "dfTT3.rds")

dygraph(df[, c(1,5,6,7)], main = "Prediction Intervals (90%)") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  #dySeries(c("Tmin", "Qobs2", "Tmax"), label = "T") %>%
  dySeries(c("T3lQ", "T3mQ", "T3uQ"), label = "T'''") %>%
  dySeries(c("Qobs"), label = "Qobs") %>%
  dyOptions(colors = brewer.pal(8, "Dark2")[c(3,4,8)]) %>%
  dyLegend(width = 500) %>%
  dyRangeSelector()

################################################################################
# STEP G: MODEL AND PARAMETER SIMILARITIES #####################################
################################################################################

library(ggplot2)

PlotModelSimilarities(ModelList = ModelList, EnsembleTable = RE,
                      synMID = 87, plotType="frequency",
                      myTitle="", modellingOptions=c("arch1","arch2",
                                                     "qsurf","qperc",
                                                     "esoil","qintf","q_tdh"))

TRUEp <- names(TRUEparams)[TRUEparams != -999]
PlotParameterSimilarities(RealisationsTable = RE, parameters = parameters,
                          selectedParams = NULL,
                          synparameters = TRUEparams,
                          labelPRE = "PRIOR", labelPOST = "POSTERIOR")

# Interquartile range for timedelay
td <- as.numeric(as.character(RE$timedelay))
median(td, na.rm = TRUE) - IQR(td, na.rm = TRUE)/2
median(td, na.rm = TRUE) + IQR(td, na.rm = TRUE)/2

################################################################################
# STEP H: CROSS-VALIDATION #####################################################
################################################################################

set.seed(0123)
parameters <- generateParameters(NumberOfRuns = 10000, SamplingType = "LHS")
B2 <- readRDS("B2.rds")
B2val <- B2[(19996/2 + 1):19996,]
# saveRDS(B2val, "B2val.rds")
TRUEmodel <- 87
TRUEparams <- readRDS("TRUEparams.rds")
Q <- fuse(DATA = B2val[,c("P", "E")], mid = 87, deltim = 1,
          ParameterSet = TRUEparams)
# Substitute synthetic Q to observed Q
DATA <- cbind(B2val[,c("P", "E")],Q)
# saveRDS(object = DATA, file = "B2trainSyntheticval.rds")

library(fuse)
library(tiger)
RE <- readRDS("RE.rds")
params <- parameters[as.numeric(as.character(RE$pid)),]
warmup <- round(dim(DATA)[1]/2,0)
pperiod <- (warmup + 1):dim(DATA)[1]
discharges <- matrix(NA, nrow=dim(RE)[1], ncol=dim(DATA)[1]-warmup)

for (i in 1:dim(RE)[1]){

  mid <- as.numeric(as.character(RE$mid[i]))
  ParameterSet <- as.list(params[i,])

  # Run FUSE Routing module
  Qsim <- fuse(DATA, mid, deltim = 1, ParameterSet)

  x <- data.frame(Qo=DATA[pperiod,"Q"], Qs=Qsim[pperiod])

  discharges[i,] <- Qsim[pperiod]

}

library(dygraphs)
library(RColorBrewer)
df <- data.frame("Dates" = seq(1, dim(discharges)[2]),
                 "Qobs" = DATA$Q[pperiod],
                 "lQ" = apply(discharges,2,quantile, probs = 0.05, na.rm=TRUE),
                 "mQ" = apply(discharges,2,quantile, probs = 0.50, na.rm=TRUE),
                 "uQ" = apply(discharges,2,quantile, probs = 0.95, na.rm=TRUE))
saveRDS(df, "CrossValidation.rds")
dygraph(df, main = "Prediction Intervals (90%)") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dySeries(c("lQ", "mQ", "uQ"), label = "T") %>%
  dySeries(c("Qobs"), label = "Qobs") %>%
  dyOptions(colors = brewer.pal(8, "Dark2")[c(2,8)]) %>%
  dyLegend(width = 500) %>%
  dyRangeSelector()

# Accuracy
indicator1 <- rep(NA,dim(df)[1])
for (t in 1:dim(df)[1]){
  indicator1[t] <- ifelse(df$Qobs[t] <= df$uQ[t] &&
                            df$Qobs[t] >= df$lQ[t],1,0)
}
round(sum(indicator1)/dim(df)[1]*100,0)

# Probabilistic NS
pNS(df$Qobs, discharges) # 98%

################################################################################
# STEP I: EXPERIMENT WITH OBSERVED DATA ########################################
################################################################################

rm(list=ls(all=TRUE))

library(fuse)
library(amca)

ObsIndicesNames <- c("LAGTIME","MAE","NSHF","NSLF")
ResultsFolder <- "observedData/"
numberOfParamSets <- 10000

DATA <- readRDS("realDATA.rds")
dates <- readRDS("B2trainSyntheticDates.rds")

warmup <- round(dim(DATA)[1]/2,0)
pperiod <- (warmup + 1):dim(DATA)[1]

# Generate parameters for simulations
set.seed(0123)
parameters <- generateParameters(NumberOfRuns = numberOfParamSets,
                                 SamplingType = "LHS")

ModelList <- PrepareTable()[seq(1,1248,2),]

# Run FUSE (on the HPC using FUSE.sh) and scan results

# Check if all the result files have been calculated:
# ScanResultFiles(ResultsFolder,ModelList)

# Run AMCA
results <- amca(DATA = DATA, ResultsFolder = ResultsFolder,
                ObsIndicesNames = ObsIndicesNames,
                selectedModels = ModelList$mid, warmup = warmup,
                verbose = TRUE, PreSel = TRUE)

library(xts)
library(dygraphs)
library(RColorBrewer)
df <- xts(data.frame("Qobs" = IE$Qobs,
                     "Tmin" = IE$minQ,
                     "Qobs2" = IE$Qobs,
                     "Tmax" = IE$maxQ,
                     "T3lQ" = T3$bounds$lQ,
                     "T3mQ" = T3$bounds$mQ,
                     "T3uQ" = T3$bounds$uQ, stringsAsFactors = FALSE),
          order.by = dates[pperiod])

dygraph(df[, c(1,5,6,7)], main = "Prediction Intervals (90%)") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  #dySeries(c("Tmin", "Qobs2", "Tmax"), label = "T") %>%
  dySeries(c("T3lQ", "T3mQ", "T3uQ"), label = "T'''") %>%
  dySeries(c("Qobs"), label = "Qobs") %>%
  dyOptions(colors = brewer.pal(8, "Dark2")[c(3,4,8)]) %>%
  dyLegend(width = 500) %>%
  dyRangeSelector()
