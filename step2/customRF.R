
### The below code was sourced from Matt Maros github repository
## https://github.com/mematt/ml4calibrated450k/blob/master/tRF/train_tunedRF.R
#--------------------------------------------------------------------
# ml4calibrated450k - tuned RF (tRF) - Train function 
#
# Matt Maros
# maros@uni-heidelberg.de
#
#--------------------------------------------------------------------


## Load required libraries, data objects 
# Check, install | load recquired packages ---------------------------------------------------------------------------------------------------------------------------

if (!requireNamespace("caret", quietly = TRUE)) { 
  install.packages("caret", dependencies = T)
  library(caret) 
} else {library(caret)}

if (!requireNamespace("randomForest", quietly = TRUE)) { 
  install.packages("randomForest")
  library(randomForest) } else {library(randomForest)}


###########################################################################################
### 1. Custom random forest function within the caret package to allow for ntree tuning ###
###########################################################################################

# Custom function within the caret package to tune ntree, mtry and node.size paramters of randomForest() 

# CustomRF for caret
customRF <- list(type = "Regression", library = "randomForest", loop = NULL)

customRF$parameters <- data.frame(parameter = c("mtry", "ntree", "nodesize"), 
                                  class = rep("numeric", 3), 
                                  label = c("mtry", "ntree", "nodesize"))

customRF$grid <- function(x, y, len = NULL, search = "grid") {}

customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, 
               mtry = param$mtry,  #tuneable parameter
               ntree=param$ntree,  #tuneable parameter
               nodesize = param$nodesize, #tuneable parameter
               ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL) predict(modelFit, newdata)

customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL) predict(modelFit, newdata, type = "prob")
