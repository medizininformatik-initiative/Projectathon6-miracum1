#load/install packages
packages <- c("rprojroot","fhircrackr","config","dplyr","zoo","stringr","tidyr")

for(package in packages){
  
  available <- suppressWarnings(require(package, character.only = T))
  
  if(!available){
    install.packages(package, repos="https://ftp.fau.de/cran/", quiet = TRUE)
  }
}
