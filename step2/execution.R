### Master script that will source the various R scripts of the pipeline 

# Install and load the needed packages
source(file.path(getwd(), "1_install_r_packages.R"))

# Selection of data from FHIR server needed for weather models
source(file.path(getwd(), "2_data_selection.R"))

# Extracting the necessary weather parameters by selecting stations 
# and create different data frames for time resolutions (daily, two-daily, weekly, monthly, ...) 
source(file.path(getwd(),"3_feature_extraction.R"))

# Modeling 1 - fits Poisson regression (baseline) and machine learning models (rf, xgb and svm) 
# Time series cross-validation: fixedWindow = FALSE
source(file.path(getwd(),"4_modeling1_fixedwindow_false.R"))

# Modeling 2 - same as Modeling 1 but with different approach for cross-validation 
# Time series cross-validation: fixedWindow = TRUE
source(file.path(getwd(),"5_modeling2_fixedwindow_true.R"))

# Modeling 3 - fits generalized additive models 
source(file.path(getwd(),"6_modeling3_gamboost.R"))