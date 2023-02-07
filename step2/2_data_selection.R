############### Clear Rstudio environment variables ####################
rm(list = ls())
gc() # optional
### Preparation

start <- Sys.time()

#load/install a packages
source("install_R_packages.R")

library(data.table)

#create directories

if(!dir.exists("Summary")){dir.create("Summary")}
if(!dir.exists("errors")){dir.create("errors")}
if(!dir.exists("Bundles")){dir.create("Bundles")}


#read  config
if(file.exists("config.yml")){
  conf <- config::get(file = "config.yml")
}else{
  conf <- config::get(file = "config_default.yml")
}

if(!(conf$ssl_verify_peer)){httr::set_config(httr::config(ssl_verifypeer = 0L))}

#update proxy configs
if((conf$http_proxy)!= ""){Sys.setenv(http_proxy  = conf$http_proxy)}
if((conf$https_proxy)!= ""){Sys.setenv(https_proxy  = conf$https_proxy)}
if((conf$no_proxy)!= ""){Sys.setenv(no_proxy  = conf$no_proxy)}

brackets = c("[", "]")
sep = " || "
############Data extraction#############################
##extract the condition resources of the list of stroke ICD's greater than 2015-01-01 and extract the associated patient and encounter resource
#To get encounters where condition is only referenced in encounter Encounter:diagnosis
#To get encounters where encounter is referenced in condition  Condition:encounter

#configure the fhir search url
encounter_request <- fhir_url(url = conf$serverbase, 
                              resource = "Condition", 
                              parameters = c("recorded-date" = "ge2015-01-01",
											"recorded-date" = "le2021-12-31",
                                             "code"="I60.0,I60.1,I60.2,I60.3,I60.4,I60.5,I60.6,I60.7,I60.8,I60.9,I61.0,I61.1,I61.2,I61.3,I61.4,I61.5,I61.6,I61.8,I61.9,I63.0,I63.1,I63.2,I63.3,I63.4,I63.5,I63.6,I63.8,I63.9,I67.80!",
                                             "_revinclude"="Encounter:diagnosis",
                                             "_include" = "Condition:encounter",
                                             "_include"="Condition:subject"
                              ))

# design parameter for Patient, Encounter, condition and procedure  resources as per fhir_crack function requirement
patients <- fhir_table_description(resource = "Patient",
                                   cols = c(patient_id = "id",
                                            gender        = "gender",
                                            birthdate     = "birthDate",
                                            patient_zip   = "address/postalCode"),
                                   style = fhir_style(sep=sep,
                                                      brackets = brackets,
                                                      rm_empty_cols = FALSE)
)



encounters <- fhir_table_description(resource = "Encounter",
                                     cols = c(encounter_id = "id",
                                              admission_date= "period/start",
                                              discharge_date= "period/end",
                                              condition_id ="diagnosis/condition/reference",
                                              patient_id = "subject/reference",
                                              patient_type_fhir_class ="class/code",
                                              diagnosis_use = "diagnosis/use/coding/code",
                                              discharge_reason = "hospitalization/dischargeDisposition/coding/code",
                                              rank = "diagnosis/rank"),
                                     style = fhir_style(sep=sep,
                                                        brackets = brackets,
                                                        rm_empty_cols = FALSE)
)


condition <- fhir_table_description(resource = "Condition",
                                    cols = c(condition_id = "id",
                                             recorded_date= "recordedDate",
                                             icd = "code/coding/code",
                                             system         = "code/coding/system",
                                             encounter_id = "encounter/reference",
                                             patient_id     = "subject/reference"),
                                    style = fhir_style(sep=sep,
                                                       brackets = brackets,
                                                       rm_empty_cols = FALSE)
)


#download the bundles bit by bit and crack them immediately
start_time <- Sys.time()
combined_tables <- list(enc=data.table(), pat = data.table(), con = data.table())

while(!is.null(encounter_request)&&length(encounter_request)>0){
  enc_bundles <- fhir_search(request = encounter_request, username = conf$user, password = conf$password, 
                             verbose = 2,log_errors = "errors/encounter_error.xml", max_bundles = conf$max_bundles)
  
  enc_tables <- fhir_crack(enc_bundles, 
                design = fhir_design(enc = encounters, pat = patients, con = condition),
                data.table = TRUE)
  
  combined_tables <- lapply(names(combined_tables), 
                       function(name){
                           rbind(combined_tables[[name]], enc_tables[[name]], fill=TRUE)
                       })
  
  names(combined_tables) <- names(enc_tables)
  #get rid of duplicates
  combined_tables$enc <- unique(combined_tables$enc)
  combined_tables$con <- unique(combined_tables$con)
  combined_tables$pat <- unique(combined_tables$pat)
  
  encounter_request <- fhir_next_bundle_url()
}
#fhir_save(bundles = enc_bundles, directory = "Bundles/Encounters")
rm(enc_bundles, enc_tables)

end_time <- Sys.time()
print(end_time - start_time)



#############################

if(nrow(combined_tables$enc) == 0){
  write("Could not find any encounter resource in the server for the required stroke condition. Query Stopped.", file ="errors/error_message.txt")
  stop("No Stroke encounters found - aborting.")
}

if(nrow(combined_tables$pat) == 0){
  write("Could not find any patient resource in the server for the required stroke condition. Query Stopped.", file ="errors/error_message.txt")
  stop("No Patients for stroke condition found - aborting.")
}

#rm(con_bundles,con_tables)


###############extract and process patient resource##############################
df.patients <- combined_tables$pat
df.patients <- fhir_rm_indices(df.patients, brackets = brackets )
df.patients <- df.patients[!duplicated(df.patients),]


################extract encounter resource###############

df.encounters <- combined_tables$enc
df.encounters <- fhir_melt(df.encounters,
                           columns = c('condition_id' ,'rank','diagnosis_use'),
                           brackets = brackets, sep = sep, all_columns = TRUE)
df.encounters <- fhir_rm_indices(df.encounters, brackets = brackets )


df.encounters$condition_id <- sub("Condition/", "", df.encounters$condition_id)
df.encounters$patient_id <- sub("Patient/", "", df.encounters$patient_id)

df.encounters$admission_date <- as.POSIXct(df.encounters$admission_date,format="%Y-%m-%dT%H:%M:%S")
df.encounters$discharge_date <- as.POSIXct(df.encounters$discharge_date,format="%Y-%m-%dT%H:%M:%S")
df.encounters <- df.encounters[!duplicated(df.encounters),]
df.encounters <- df.encounters[c(which(df.encounters$admission_date >= '2015-01-01')),]

df.encounters.trunc <- subset(df.encounters,select = c(patient_id,encounter_id,admission_date,discharge_date))
df.encounters.trunc <- df.encounters.trunc[!duplicated(df.encounters.trunc),]


####################extract the condition resources##################

df.conditions <- combined_tables$con
df.conditions <- fhir_melt(df.conditions, columns = c("icd", "system"), brackets = brackets, sep = sep, all_columns = TRUE)
df.conditions <- fhir_melt(df.conditions, columns = c("icd", "system"), brackets = brackets, sep = sep, all_columns = TRUE)

df.conditions <- fhir_rm_indices(df.conditions, brackets = brackets )
df.conditions$encounter_id <- sub("Encounter/", "", df.conditions$encounter_id)
df.conditions$patient_id <- sub("Patient/", "", df.conditions$patient_id)

df.conditions <- df.conditions[grepl("icd-10", system)]

icd_codes <- c('I60.0','I60.1','I60.2','I60.3','I60.4','I60.5','I60.6','I60.7','I60.8','I60.9'
               ,'I61.0','I61.1','I61.2',  'I61.3','I61.4','I61.5','I61.6','I61.8','I61.9'
               ,'I63.0','I63.1','I63.2','I63.3','I63.4','I63.5','I63.6','I63.8','I63.9','I67.80!')

df.conditions <- df.conditions[c(which(df.conditions$icd %in% icd_codes) )]

df.conditions$recorded_date <- as.POSIXct(df.conditions$recorded_date ,format="%Y-%m-%dT%H:%M:%S")
df.conditions <- df.conditions[!duplicated(df.conditions),]


df.encounters.subset <-  df.encounters[,c("encounter_id", "condition_id")]
setnames(df.encounters.subset,old = c("encounter_id", "condition_id"),new = c("encounter.encounter_id", "encounter.condition_id"))
df.conditions <- merge.data.table(x = df.conditions, 
                               y = df.encounters.subset ,
                               by.x = "condition_id",
                               by.y = "encounter.condition_id",
                               all.x = TRUE)

df.conditions[is.na(encounter_id),encounter_id:=encounter.encounter_id]
df.conditions[, encounter.encounter_id:=NULL]



############### Join all the resources ##################################################
#join encounters to conditions that were referenced in an encounter
df.cohort1 <- left_join(df.conditions[condition_id %in% df.encounters$condition_id], 
                        df.encounters[!is.na(condition_id), 
                                      c("condition_id", "admission_date", "discharge_date", "patient_type_fhir_class", "rank", "diagnosis_use", "discharge_reason")], 
                        "condition_id")

# join encounters to conditions that were not referenced in an encounter
df.cohort2 <- left_join(df.conditions[!condition_id %in% df.encounters$condition_id],
                        unique(df.encounters[, c("encounter_id", "admission_date", "discharge_date", "patient_type_fhir_class", "discharge_reason")]),
                        "encounter_id")

# rbind them
df.cohort <- rbind(df.cohort1, df.cohort2, fill = T)
df.cohort <- left_join(df.cohort, df.patients, "patient_id")
df.cohort$icd_family <- substr(df.cohort$icd, 1, 3)
df.cohort <- df.cohort[,c("patient_id", "birthdate","gender","patient_zip",
                          "encounter_id", "admission_date", "icd_family", "discharge_date",
                          "recorded_date", "system", "patient_type_fhir_class",
                          "rank", "diagnosis_use")]

############### Save data as csv ####################################
df.cohort <- distinct(df.cohort)
write.csv(df.cohort,file  = file.path(getwd(),"data/stroke_cohort.csv"))
print("Data selection done")
