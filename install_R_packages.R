#load/install packages
packages <- c("fhircrackr","config","dplyr","zoo","stringr","tidyr")

for(package in packages){
  
  available <- suppressWarnings(require(package, character.only = T))
  
  if(!available){
    install.packages(package, quiet = TRUE)
  }
}
