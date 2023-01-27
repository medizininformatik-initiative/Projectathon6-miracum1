#load/install packages
packages <- c("fhircrackr","config","dplyr",
              "zoo","stringr","tidyr","data.table",
              "openxlsx","rdwd","lubridate","geosphere",
              "stringi","caret","xgboost","ranger","mboost",
              "tidyverse","corrplot","earth","Metrics","ggplot2",
			  "RANN", "randomForest", "kernlab", "pacman")

for(package in packages){
  
  available <- suppressWarnings(require(package, character.only = T))
  
  if(!available){
    install.packages(package, quiet = TRUE)
    available <- suppressWarnings(require(package, character.only = T))
  }
}
print("Installation done")