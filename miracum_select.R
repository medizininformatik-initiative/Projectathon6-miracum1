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
code_systems <-
  list(icd = "http://fhir.de/CodeSystem/bfarm/icd-10-gm",
       ops = "http://fhir.de/CodeSystem/bfarm/ops",
       loinc = "http://loinc.org")
codes <- list(
  icd = c("I60.0","I60.1","I60.2","I60.3","I60.4","I60.5","I60.6","I60.7","I60.8","I60.9","I61.0","I61.1","I61.2","I61.3","I61.4","I61.5","I61.6","I61.8","I61.9","I63.0","I63.1","I63.2","I63.3","I63.4","I63.5","I63.6","I63.8","I63.9","I67.80!"),
  ops = c("8-020.8","8-020.D","8-980","8-981","8-981.2","8-981.20","8-981.21","8-981.22","8-981.23","8-981.3","5-025","5-026","5-026.4","8-83B.8","8-84B.0","8-84B.2","8-84B.3","8-84B.4","8-84B.5","8-706","8-713.0","8-980","8-98F","8-98B"),
  loinc = c("777-3","26515-7","778-1","49497-1","32207-3","51631-0","32623-1","28542-9","6301-6","34714-6","5894-1","3173-2","14979-9","3243-3","14182-0","3255-7","48664-7","7799-0","48067-3","48065-7","2160-0","14682-9","62238-1","50210-4","77147-7","50384-7","2951-2","2823-3","2075-0","2601-3","14798-3","2498-4","3034-6","2276-4","20567-4","789-8","26453-1","790-6","787-2","30428-7","718-7","59260-0","55782-7","20509-6","20570-8","4544-3","71833-8","31100-1","2093-3","14647-2","18262-6","69419-0","2089-1","49132-4","13457-7","22748-8","39469-2","2085-9","14646-4","49130-8","2571-8","14927-8","1751-7","61151-7","2862-1","61152-5","54347-0","2345-7","14749-6","2339-0","2341-6","2340-8","15074-8","41651-1","39481-7","41652-9","39480-9","17856-6","4549-2","59261-8","4548-4","17855-8")
)

############Data extraction#############################
##extract the condition resources of the list of stroke ICD's greather than 2015-01-01 and extract the associated patient, encounter, condition and procedure resource
#configure the fhir search url
encounter_request <- fhir_url(
  url = conf$serverbase,
  resource = "Encounter",
  parameters = c(
    "date" = "ge2015-01-01",
    "diagnosis:Condition.code" = paste(paste0(code_systems$icd, "|", codes$icd), collapse = ","),
    "_include" = "Encounter:patient",
    "_include" = "Encounter:diagnosis"
  )
)

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
                             verbose = 2,log_errors = "errors/encounter_error.xml", max_bundles = 100)
  
  enc_tables <- fhir_crack(enc_bundles, 
                design = fhir_design(enc = encounters, pat = patients, con = condition),
                data.table = TRUE)
  
  combined_tables <- lapply(names(combined_tables), 
                       function(name){
                           rbind(combined_tables[[name]], enc_tables[[name]], fill=TRUE)
                       })
  
  names(combined_tables) <- names(enc_tables)
  
  encounter_request <- fhir_next_bundle_url()
}
#fhir_save(bundles = enc_bundles, directory = "Bundles/Encounters")
rm(enc_bundles, enc_tables)

end_time <- Sys.time()
print(end_time - start_time)



#extract condition resource based on the list of ICD and include Condition:encounter, Condition:subject/patient


### extraction for covering the cases who doesn't have a link of condition in the encounter resource
condition_request_2 <- fhir_url(url = conf$serverbase, 
                              resource = "Condition", 
                              parameters = c("recorded-date" = "ge2015-01-01",
                                             "code" = paste(paste0(code_systems$icd, "|", codes$icd), collapse = ","),
                                             "_include" = "Condition:encounter",
                                             "_include"="Condition:subject"
                              ))

#Download in 100er batches and crack immediately, then append to combined tables from above
start_time <- Sys.time()
while(!is.null(condition_request_2)&&length(condition_request_2)>0){
  con_bundles <- fhir_search(request = condition_request_2, username = conf$user, password = conf$password, 
                             verbose = 2,log_errors = "errors/encounter_error.xml", max_bundles = 100)
  
  con_tables <- fhir_crack(con_bundles, 
                           design = fhir_design(enc = encounters, pat = patients, con = condition),
                           data.table = TRUE)
  
  combined_tables <- lapply(names(combined_tables), 
                            function(name){
                              rbind(combined_tables[[name]], con_tables[[name]], fill=TRUE)
                            })
  
  names(combined_tables) <- names(con_tables)
  
  #get rid of duplicates
  combined_tables$enc <- unique(combined_tables$enc)
  combined_tables$con <- unique(combined_tables$con)
  combined_tables$pat <- unique(combined_tables$pat)
  
  condition_request_2 <- fhir_next_bundle_url()
}

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

rm(con_bundles,con_tables)


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

# icd_codes <- c('I60.0','I60.1','I60.2','I60.3','I60.4','I60.5','I60.6','I60.7','I60.8','I60.9'
#                ,'I61.0','I61.1','I61.2',  'I61.3','I61.4','I61.5','I61.6','I61.8','I61.9'
#                ,'I63.0','I63.1','I63.2','I63.3','I63.4','I63.5','I63.6','I63.8','I63.9','I67.80!')
# icd_codes <- codes$icd

df.conditions <- df.conditions[c(which(df.conditions$icd %in% codes$icd) )]

df.conditions$recorded_date <- as.POSIXct(df.conditions$recorded_date ,format="%Y-%m-%dT%H:%M:%S")

df.encounters.subset <-  df.encounters[,c("encounter_id", "condition_id")]
setnames(df.encounters.subset,old = c("encounter_id", "condition_id"),new = c("encounter.encounter_id", "encounter.condition_id"))
df.conditions <- merge.data.table(x = df.conditions, 
                               y = df.encounters.subset ,
                               by.x = "condition_id",
                               by.y = "encounter.condition_id",
                               all.x = TRUE)

df.conditions[is.na(encounter_id),encounter_id:=encounter.encounter_id]
df.conditions[, encounter.encounter_id:=NULL]


####################extract the procedure resources##################
#df.procedure <- enc_tables$proc

#if procedure resources is empty, extract it based on procedure id


patient_ids <- unique(df.encounters$patient_id)
nchar_for_ids <- 1800 - nchar(conf$serverbase)
n <- length(patient_ids)
list <- split(patient_ids, ceiling(seq_along(patient_ids)/n)) 
nchar <- sapply(list, function(x){sum(nchar(x))+(length(x)-1)}) 

#reduce the chunk size until number of characters is small enough
while(any(nchar > nchar_for_ids)){
  n <- n/2
  list <- split(patient_ids, ceiling(seq_along(patient_ids)/n))
  nchar <- sapply(list, function(x){sum(nchar(x))+(length(x)-1)})
}

procedure <- fhir_table_description(resource = "Procedure",
                                    cols = c(procedure_id = "id",
                                             performed_date= "performedDateTime",
                                             ops = "code/coding/code",
                                             system         = "code/coding/system",
                                             #  encounter_id = "encounter/reference",
                                             patient_id     = "subject/reference"),
                                    style = fhir_style(sep=sep,
                                                       brackets = brackets,
                                                       rm_empty_cols = FALSE)
)

df.procedure <<- data.table()

procedure_list  <- lapply(list, function(x){
  
  ids <- paste(x, collapse = ",")
  procedure_req <- fhir_url(url = conf$serverbase,
                          resource = "Procedure",
                          parameters = c(subject = ids
                                         ))
  
  proc_bundles <- fhir_search(procedure_req,
                             username = conf$username,
                             password = conf$password,
                             log_errors = "errors/Procedure_error.xml")
  
  
  proc_pat_table <- fhir_crack(proc_bundles,design = procedure)
  df.procedure <<- rbind(df.procedure, proc_pat_table, fill=TRUE)
})

if(nrow(df.procedure) > 0){
  df.procedure <- fhir_rm_indices(df.procedure, brackets = brackets )
  df.procedure$patient_id <- sub("Patient/", "", df.procedure$patient_id)
  
  
  
  #Filter resources with needed ops code 
  # ops_codes <- c("8-020.8|8-020.D|8-980|8-981|8-981.2|8-981.20|8-981.21|8-981.22|8-981.23|8-981.3|5-025|5-026|5-026.4|8-83B.8|8-84B.0|8-84B.2|8-84B.3|8-84B.4|8-84B.5|8-706|8-713.0|8-980|8-98F|8-98B")
  # ops_codes <- codes$ops
  
  df.procedure <- df.procedure %>% filter(str_detect(ops, paste(codes$ops, collapse = "|")))
  df.procedure$performed_date <- as.POSIXct(df.procedure$performed_date ,format="%Y-%m-%dT%H:%M:%S")
  df.procedure <- setDT(df.procedure)[setDT(df.encounters.trunc), 
               on = .(patient_id, performed_date >= admission_date, performed_date < discharge_date), 
               encounter_id := encounter_id][]
  
  if(length(which(is.na(df.procedure$encounter_id)))>0)
  {
    df.procedure <- df.procedure[-c(which(is.na(df.procedure$encounter_id))),]
  }
  
  if(nrow(df.procedure) > 0){
  
        #Features
      df.procedure$features <- ""
      
      #Intravenous lyse therapy 
      df.procedure$features[c(which(startsWith(df.procedure$ops,c("8-020.8"))))] <- "IVT"
      df.procedure$features[c(which(startsWith(df.procedure$ops,c("8-020.D"))))] <- "IVT"
      
      #Admission to ICU
      df.procedure$features[c(which(startsWith(df.procedure$ops,"8-980")))] <- "Admission to ICU"
      df.procedure$features[c(which(startsWith(df.procedure$ops,"8-98F")))] <- "Admission to ICU"
      
      #Admission to stroke unit
      df.procedure$features[c(which(startsWith(df.procedure$ops,"8-981")))] <- "Admission to stroke unit"
      
      #Neurosurgery
      df.procedure$features[c(which(startsWith(df.procedure$ops,c("5-025"))))] <- "Neurosurgery"
      df.procedure$features[c(which(startsWith(df.procedure$ops,c("5-026"))))] <- "Neurosurgery"
      
      
      #Thrombectomy
      df.procedure$features[c(which(startsWith(df.procedure$ops,c("8-83B.8"))))] <- "Thrombectomy"
      
      #Intrakraniell Stent
      df.procedure$features[c(which(startsWith(df.procedure$ops,c("8-84B"))))] <- "Intrakraniell Stent"
      
      #Ventilation
      df.procedure$features[c(which(startsWith(df.procedure$ops,c("8-706"))))] <- "Mechanical Ventilation"
      df.procedure$features[c(which(startsWith(df.procedure$ops,c("8-713"))))] <- "Mechanical Ventilation"
      
      
      df.procedure.wide <- df.procedure%>%
      group_by(patient_id,encounter_id,features)%>%
      summarise(ops = paste(unique(ops), collapse = ','))
        
      df.procedure.wide <- tidyr:::pivot_wider(data = df.procedure.wide,
                                         names_from = features,
                                         values_from = ops,
                                         id_cols = c(encounter_id,patient_id))

      
  }
  
  
} #endif
#######################################################################################################################


#######################################################################################################################
#extract observation resource for the required patient ids and loinc codes

patient_ids <- unique(df.encounters$patient_id)
# loincs_string <- "777-3,26515-7,778-1,49497-1,32207-3,51631-0,32623-1,28542-9,6301-6,34714-6,5894-1,3173-2,14979-9,3243-3,14182-0,3255-7,48664-7,7799-0,48067-3,48065-7,2160-0,14682-9,62238-1,50210-4,77147-7,50384-7,2951-2,2823-3,2075-0,2601-3,14798-3,2498-4,3034-6,2276-4,20567-4,789-8,26453-1,790-6,787-2,30428-7,718-7,59260-0,55782-7,20509-6,20570-8,4544-3,71833-8,31100-1,2093-3,14647-2,18262-6,69419-0,2089-1,49132-4,13457-7,22748-8,39469-2,2085-9,14646-4,49130-8,2571-8,14927-8,1751-7,61151-7,2862-1,61152-5,54347-0,2345-7,14749-6,2339-0,2341-6,2340-8,15074-8,41651-1,39481-7,41652-9,39480-9,17856-6,4549-2,59261-8,4548-4,17855-8"
nchar_loincs <- nchar(paste(paste0(code_systems$loinc, "|", codes$loinc), collapse = ","))
nchar_for_ids <- 900 - (nchar(conf$serverbase)+nchar_loincs)
n <- length(patient_ids)
list <- split(patient_ids, ceiling(seq_along(patient_ids)/n)) 
nchar <- sapply(list, function(x){sum(nchar(x))+(length(x)-1)}) 

#reduce the chunk size until number of characters is small enough
while(any(nchar > nchar_for_ids)){
  n <- n/2
  list <- split(patient_ids, ceiling(seq_along(patient_ids)/n))
  nchar <- sapply(list, function(x){sum(nchar(x))+(length(x)-1)})
}

observation <- fhir_table_description(resource = "Observation",
                                      cols = c(observation_id = "id",
                                               effective_date= "effectiveDateTime",
                                               loinc_code = "code/coding/code",
                                               system         = "code/coding/system",
                                               display = "code/coding/display",
                                               value =  "valueQuantity/value",
                                               unit =  "valueQuantity/unit",
                                               #encounter_id = "encounter/reference",
                                               patient_id     = "subject/reference"),
                                      style = fhir_style(sep=sep,
                                                         brackets = brackets,
                                                         rm_empty_cols = FALSE)
)

df.observation <<- data.table()

time_tmp_start <- Sys.time()

# observation_list  <- lapply(list, function(x){
for (name in names(list)) {
  x <- list[[name]]
  
  DIZtools::time_diff_print(
    older_timestamp = time_tmp_start,
    iteration = name,
    iterations = length(list)
  )
  
  
  ids <- paste(x, collapse = ",")
  
  
  ##
  obs_request <- fhir_url(url = conf$serverbase,
                          resource = "Observation",
                          parameters = c(subject = ids
                                         ,"code" = paste(paste0(code_systems$loinc, "|", codes$loinc), collapse = ",")))
                                         # ,"code" = "777-3,26515-7,778-1,49497-1,32207-3,51631-0,32623-1,28542-9,6301-6,34714-6,5894-1,3173-2,14979-9,3243-3,14182-0,3255-7,48664-7,7799-0"))
  obs_bundles <- fhir_search(obs_request,
                             username = conf$username,
                             password = conf$password,
                             log_errors = "errors/Observations_error.xml")

  obs_table <- fhir_crack(obs_bundles, design = observation)
  df.observation <<- rbind(df.observation, obs_table, fill = TRUE)
  
}
# )

print("Finished extracting the Observation ressources. Starting to analyze them.")

#process observations_raw resources
if(nrow(df.observation) > 0){
  df.observation <- fhir_rm_indices(df.observation, brackets = brackets )

  df.observation$patient_id <- sub("Patient/", "", df.observation$patient_id)  
  
  df.observation$effective_date <- as.POSIXct(df.observation$effective_date ,format="%Y-%m-%dT%H:%M:%S")
  df.observation <- setDT(df.observation)[setDT(df.encounters.trunc), 
                                      on = .(patient_id, effective_date >= admission_date, effective_date < discharge_date), 
                                      encounter_id := encounter_id][]
  
  if(length(which(is.na(df.observation$encounter_id)))>0)
  {
    df.observation <- df.observation[-c(which(is.na(df.observation$encounter_id))),]
  }
  
}
#######################################################################################################################
#extract the diagnosis resource based on patient ids

patient_ids <- unique(df.encounters$patient_id)
nchar_for_ids <- 1800 - nchar(conf$serverbase)
n <- length(patient_ids)
list <- split(patient_ids, ceiling(seq_along(patient_ids)/n)) 
nchar <- sapply(list, function(x){sum(nchar(x))+(length(x)-1)}) 

#reduce the chunk size until number of characters is small enough
while(any(nchar > nchar_for_ids)){
  n <- n/2
  list <- split(patient_ids, ceiling(seq_along(patient_ids)/n))
  nchar <- sapply(list, function(x){sum(nchar(x))+(length(x)-1)})
}


condition <- fhir_table_description(resource = "Condition",
                                    cols = c(condition_id = "id",
                                             recorded_date= "recordedDate",
                                             icd = "code/coding/code",
                                             system         = "code/coding/system",
                                             patient_id     = "subject/reference"),
                                    style = fhir_style(sep=sep,
                                                       brackets = brackets,
                                                       rm_empty_cols = FALSE)
)

df.conditions.previous <<- data.table()

condition_list  <- lapply(list, function(x){
  
  ids <- paste(x, collapse = ",")
  cond_request <- fhir_url(url = conf$serverbase,
                           resource = "Condition",
                           parameters = c(subject = ids)
                           )
  
  cond_bundles <- fhir_search(cond_request,
                              username = conf$username,
                              password = conf$password,
                              log_errors = "errors/diagnosis_error.xml")
  
  cond_table <- fhir_crack(cond_bundles,design = condition)
  df.conditions.previous <<- rbind(df.conditions.previous, cond_table, fill=TRUE)
  
}) # lapply


if(nrow(df.conditions.previous) > 0){
  #process observations_raw resources
  df.conditions.previous <- fhir_rm_indices(df.conditions.previous, brackets = brackets)
  df.conditions.previous$patient_id <-sub("Patient/", "", df.conditions.previous$patient_id) 
  
  df.conditions.previous$recorded_date <- as.POSIXct( df.conditions.previous$recorded_date ,format="%Y-%m-%dT%H:%M:%S")

  df.conditions.previous <- left_join(df.conditions.previous,df.encounters.trunc,"patient_id")
  df.conditions.previous <- df.conditions.previous[c(which(df.conditions.previous$recorded_date < df.conditions.previous$admission_date)),]
  
  if(nrow(df.conditions.previous)>0){
    
    df.conditions.previous$features <- ""
    
    df.conditions.previous$features[c(which(startsWith(df.conditions.previous$icd,"I48")))]<- "Atrial Fibrilliation"
    
    df.conditions.previous$features[c(which(startsWith(df.conditions.previous$icd,"E78")))]<- "Hyperlipidaemia"
    
    df.conditions.previous$features[c(which(startsWith(df.conditions.previous$icd,"I63")))]<- "Stroke"
    df.conditions.previous$features[c(which(startsWith(df.conditions.previous$icd,"I61")))]<- "Stroke"
    df.conditions.previous$features[c(which(startsWith(df.conditions.previous$icd,"I60")))]<- "Stroke"
    df.conditions.previous$features[c(which(startsWith(df.conditions.previous$icd,"I67.80")))]<- "Stroke"
    
    df.conditions.previous$features[c(which(startsWith(df.conditions.previous$icd,"I10")))]<- "Hypertension"
    df.conditions.previous$features[c(which(startsWith(df.conditions.previous$icd,"I11")))]<- "Hypertension"
    df.conditions.previous$features[c(which(startsWith(df.conditions.previous$icd,"I12")))]<- "Hypertension"
    df.conditions.previous$features[c(which(startsWith(df.conditions.previous$icd,"I13")))]<- "Hypertension"
    df.conditions.previous$features[c(which(startsWith(df.conditions.previous$icd,"I14")))]<- "Hypertension"
    df.conditions.previous$features[c(which(startsWith(df.conditions.previous$icd,"I15")))]<- "Hypertension"
    
    df.conditions.previous$features[c(which(startsWith(df.conditions.previous$icd,"I20")))]<- "Myocardial infarct"
    df.conditions.previous$features[c(which(startsWith(df.conditions.previous$icd,"I21")))]<- "Myocardial infarct"
    df.conditions.previous$features[c(which(startsWith(df.conditions.previous$icd,"I22")))]<- "Myocardial infarct"
    df.conditions.previous$features[c(which(startsWith(df.conditions.previous$icd,"I23")))]<- "Myocardial infarct"
    df.conditions.previous$features[c(which(startsWith(df.conditions.previous$icd,"I24")))]<- "Myocardial infarct"
    df.conditions.previous$features[c(which(startsWith(df.conditions.previous$icd,"I25")))]<- "Myocardial infarct"
    
    
    df.conditions.previous$features[c(which(startsWith(df.conditions.previous$icd,"E10")))]<- "Diabetes mellitus"
    df.conditions.previous$features[c(which(startsWith(df.conditions.previous$icd,"E11")))]<- "Diabetes mellitus"
    df.conditions.previous$features[c(which(startsWith(df.conditions.previous$icd,"E14")))]<- "Diabetes mellitus"
    
    
    df.conditions.previous$features[c(which(startsWith(df.conditions.previous$icd,"F17.2")))]<- "Smoking"
    
    
    df.conditions.previous <- df.conditions.previous[-c(which(df.conditions.previous$features == "")), ]
    
    df.conditions.previous.wide <- df.conditions.previous%>%
      group_by(patient_id,features)%>%
      summarise(icd = paste(unique(icd), collapse = ','))
    
    df.conditions.previous.wide <- tidyr:::pivot_wider(data = df.conditions.previous.wide
                                               ,names_from = features
                                               ,values_from = icd
                                               ,id_cols = c(patient_id))
  }
  
} # endif




#######################################################################################################################
#extract medicationstatement resource for the required encounter ids and atc codes
encouter_ids <- unique(df.encounters$encounter_id)
nchar_for_ids <- 1800 - nchar(conf$serverbase)
n <- length(encouter_ids)
list <- split(encouter_ids, ceiling(seq_along(encouter_ids)/n)) 
nchar <- sapply(list, function(x){sum(nchar(x))+(length(x)-1)}) 

#reduce the chunk size until number of characters is small enough
while(any(nchar > nchar_for_ids)){
  n <- n/2
  list <- split(encouter_ids, ceiling(seq_along(encouter_ids)/n))
  nchar <- sapply(list, function(x){sum(nchar(x))+(length(x)-1)})
}


#extract medicationstatements
medstat <- fhir_table_description(resource = "MedicationStatement",
                                  cols = c(MedicationStatement_id = "id",
                                           effective_date= "effectiveDateTime",
                                           medication_id = "medicationReference/reference",
                                           encounter_id = "context/reference",
                                           patient_id     = "subject/reference"),
                                  style = fhir_style(sep=sep,
                                                     brackets = brackets,
                                                     rm_empty_cols = FALSE)
)

df.medstatement <<- data.table()

medstat_list  <- lapply(list, function(x){
  
  ids <- paste(x, collapse = ",")
  
  medstat_Request <- fhir_url(url = conf$serverbase,
                              resource = "MedicationStatement",
                              parameters = c(context = ids
                              ))
  
  medstat_bundle <- fhir_search(medstat_Request,
                                username = conf$username,
                                password = conf$password,
                                log_errors = "errors/MedicationStatement_error.xml")
  
  medstat_table <- fhir_crack(medstat_bundle,design = medstat)
  df.medstatement <<- rbind(df.medstatement, medstat_table, fill=TRUE)
  
}) # lapply()


if(nrow(df.medstatement)>0){
  #process Medication statement  resources
  df.medstatement <- fhir_rm_indices(df.medstatement, brackets = brackets )
  df.medstatement$encounter_id <-sub("Encounter/", "", df.medstatement$encounter_id)
  df.medstatement$patient_id <-sub("Patient/", "", df.medstatement$patient_id) 
  df.medstatement$medication_id <-sub("Medication/", "", df.medstatement$medication_id) 
  
  
  
  ###extract the actual medication using the IDs
  medication_ids <- unique(df.medstatement$medication_id)
  
  nchar_for_ids <- 1800 - nchar(conf$serverbase)
  n <- length(medication_ids)
  list <- split(medication_ids, ceiling(seq_along(medication_ids)/n)) 
  nchar <- sapply(list, function(x){sum(nchar(x))+(length(x)-1)}) 
  
  #reduce the chunk size until number of characters is small enough
  while(any(nchar > nchar_for_ids)){
    n <- n/2
    list <- split(medication_ids, ceiling(seq_along(medication_ids)/n))
    nchar <- sapply(list, function(x){sum(nchar(x))+(length(x)-1)})
  }
  
  #extract Medications
  medication <- fhir_table_description(resource = "Medication",
                                       cols = c(medication_id = "id",
                                                code = "code/coding/code",
                                                system = "code/coding/system"
                                                #,display = "code/coding/display"
                                       ),
                                       style = fhir_style(sep=sep,
                                                          brackets = brackets,
                                                          rm_empty_cols = FALSE)
  )
  df.medication <<- data.table()
  
  med_list  <- lapply(list, function(x){
    
    ids <- paste(x, collapse = ",")
    
    med_request <- fhir_url(url = conf$serverbase,
                            resource = "Medication",
                            parameters = c("_id" = ids)
    )
    
    medication_bundle <- fhir_search(med_request,
                                  username = conf$username,
                                  password = conf$password,
                                  log_errors = "errors/medication_error.xml")
    
  
    med_table <- fhir_crack(medication_bundle,design = medication)
    df.medication <<- rbind(df.medication, med_table)
  }) # lapply()
  
  if(exists("df.medication") && nrow(df.medication) > 0){
    
    #process Medication statement  resources
    df.medication <- fhir_rm_indices(df.medication, brackets = brackets )
    df.medstatement <- left_join(df.medstatement,df.medication,"medication_id")
  } # endif
  
} #endif


####################################join all the resources ##################################################
#df.cohort <- left_join(df.conditions,df.encounters[,c("condition_id","admission_date","discharge_date","patient_type","rank","discharge_reason")],"condition_id")
#join encounters to conditions that were referenced in an encounter
df.cohort1 <- left_join(df.conditions[condition_id %in% df.encounters$condition_id],df.encounters[!is.na(condition_id),c("condition_id","admission_date","discharge_date","patient_type_fhir_class","rank","diagnosis_use","discharge_reason")],"condition_id")

#join encounters to conditions that were not referenced in an encounter 
df.cohort2 <- left_join(df.conditions[!condition_id %in% df.encounters$condition_id],unique(df.encounters[,c("encounter_id","admission_date","discharge_date","patient_type_fhir_class","discharge_reason")]),"encounter_id")

#rbind them
df.cohort <- rbind(df.cohort1, df.cohort2, fill=T)

df.cohort <- left_join(df.cohort,df.patients,"patient_id")

df.cohort <- df.cohort[,c("patient_id","birthdate","gender","patient_zip"
                          ,"encounter_id","admission_date","discharge_date"
                          ,"recorded_date","icd","system"
                          ,"patient_type_fhir_class","rank","diagnosis_use")]

df.cohort$icd_family <- substr(df.cohort$icd,1,3)


df.cohort <- df.cohort%>%
        group_by(patient_id,encounter_id)%>%
        mutate(rank_indicator = min(rank))

#update rank mapping


df.cohort$rank_indicator[c(which(df.cohort$rank_indicator ==1 ))] <- "Primary" 
df.cohort$rank_indicator[c(which(df.cohort$rank_indicator ==2 ))] <- "Secondary" 



if(exists("df.procedure.wide")){
  df.cohort <- left_join(df.cohort,subset(df.procedure.wide,select = -c(patient_id)),"encounter_id")
}

if(exists("df.conditions.previous.wide")){
  df.cohort <- left_join(df.cohort,df.conditions.previous.wide,"patient_id")
}



###generate summary data and export####

#export to excel
df.cohort <- distinct(df.cohort)
wb <-openxlsx:::createWorkbook()
###########################cohort summary###########################
if(nrow(df.cohort) > 0){
  df.cohort.trunc <- subset(df.cohort,select = c("patient_id", "encounter_id","birthdate" 
                                                     ,"gender","patient_zip", "admission_date"
                                                     ,"discharge_date","recorded_date"
                                                     ,"icd" ,"patient_type_fhir_class"
                                                     ,"rank","diagnosis_use"))
  df.cohort.trunc <- df.cohort.trunc[!with(df.cohort.trunc,is.na(admission_date)& is.na(recorded_date)),]
  df.cohort.trunc$year_month <- substr(coalesce(as.character(df.cohort.trunc$admission_date),as.character(df.cohort.trunc$recorded_date)),1,7)

#1. ################################################################################
  #year and monthly count
  df.cohort.trunc.summary <- df.cohort.trunc %>%
    group_by(year_month) %>%
    summarise_all(funs(paste((sum(!is.na(.))/n())*100,"%")))
  
  openxlsx:::addWorksheet(wb, "Cohort_Feature_Availability")
  openxlsx:::writeDataTable(wb = wb,x = df.cohort.trunc.summary,sheet = "Cohort_Feature_Availability", withFilter = FALSE)

#2. ################################################################################
#summary based on patient class(IMP/AMB) and diag use 
  patclass.diaguse <- df.cohort%>%
    group_by(patient_type_fhir_class
             ,diagnosis_use = diagnosis_use)%>%
    summarise(count_unique_patient = length(unique(patient_id)), 
              count_unique_encounters = length(unique(encounter_id)),
              all_encounters = length(encounter_id),
              diag_i60_encounters = length(unique(encounter_id[which(icd_family == 'I60')])),
              diag_i61_encounters = length(unique(encounter_id[which(icd_family == 'I61')])),
              diag_i63_encounters = length(unique(encounter_id[which(icd_family == 'I63')]))
              )
  patclass.diaguse[is.na(patclass.diaguse)] <- "NA"
  openxlsx:::addWorksheet(wb, "PatientClass_DiagnosisUse")
  openxlsx:::writeDataTable(wb = wb,x = patclass.diaguse,sheet = "PatientClass_DiagnosisUse", withFilter = FALSE)
  
#3. #################################################################################    
  #summary based on patient class(IMP/AMB) and diag rank   
  patclas.rank <- df.cohort%>%
    group_by(patient_type_fhir_class
             ,diagnosis_rank = rank_indicator)%>%
    summarise(count_unique_patient = length(unique(patient_id)), 
              count_unique_encounters = length(unique(encounter_id)),
              all_encounters = length(encounter_id),
              diag_i60_encounters = length(unique(encounter_id[which(icd_family == 'I60')])),
              diag_i61_encounters = length(unique(encounter_id[which(icd_family == 'I61')])),
              diag_i63_encounters = length(unique(encounter_id[which(icd_family == 'I63')]))
    )   
  patclas.rank[is.na(patclas.rank)] <- "NA"
  openxlsx:::addWorksheet(wb, "PatientClass_Rank")
  openxlsx:::writeDataTable(wb = wb,x = patclas.rank,sheet = "PatientClass_Rank", withFilter = FALSE)

#4. ################################################################################
  #summary based on patient class(IMP/AMB), diag use and diag rank 
  patclass.rank.diaguse <- df.cohort%>%
    group_by(patient_type_fhir_class
             ,diagnosis_rank = rank_indicator
             ,diagnosis_use = diagnosis_use)%>%
    summarise(count_unique_patient = length(unique(patient_id)), 
              count_unique_encounters = length(unique(encounter_id)),
              all_encounters = length(encounter_id),
              diag_i60_encounters = length(unique(encounter_id[which(icd_family == 'I60')])),
              diag_i61_encounters = length(unique(encounter_id[which(icd_family == 'I61')])),
              diag_i63_encounters = length(unique(encounter_id[which(icd_family == 'I63')]))
              ) 
  patclass.rank.diaguse[is.na(patclass.rank.diaguse)] <- "NA"
  openxlsx:::addWorksheet(wb, "PatientClass_Rank_DiagnosisUse")
  openxlsx:::writeDataTable(wb = wb,x = patclass.rank.diaguse,sheet = "PatientClass_Rank_DiagnosisUse", withFilter = FALSE)

#5. #################################################################################  
  #summary based on number of visits and diag rank for patients
  df.pat.cases <- df.cohort%>%
    group_by(patient_id)%>%
    summarise(diag_rank = paste(sort(unique(rank_indicator)), collapse = ',') 
              ,number_of_visits =length(unique((encounter_id))))%>%
    group_by(diag_rank,number_of_visits)%>%
    summarise(count_unique_patient = length(unique(patient_id)))
  
  df.pat.cases[is.na(df.pat.cases)] <- "NA"
  openxlsx:::addWorksheet(wb, "Multiple_Patient_Visit")
  openxlsx:::writeDataTable(wb = wb,x = df.pat.cases,sheet = "Multiple_Patient_Visit", withFilter = FALSE)

#6. #################################################################################    
  # count of cases from each PLZ
  df.plz <- df.cohort.trunc %>%
              group_by(patient_zip)%>%
    summarise(count_encounters = length(unique(encounter_id)))%>%
      arrange(desc(count_encounters))
  
  df.plz$count_encounters[c(which(df.plz$count_encounters < 5))] <- "< 5"
  df.plz[is.na(df.plz)] <- "NA"
  openxlsx:::addWorksheet(wb, "PLZ")
  openxlsx:::writeDataTable(wb = wb,x = df.plz,sheet = "PLZ", withFilter = FALSE)

#7. #################################################################################    
  #group by and get count for each ICD
  df.conditions.summary <- df.cohort %>%
    group_by(icd) %>%
    summarise(count_encounters = length(unique(encounter_id)) 
          ,count_encounters = count_encounters  + ifelse(test = count_encounters>5,sample(-5:5, 1,replace = TRUE),sample(0:5, 1,replace = TRUE))
          ,percent_encounters = paste0(ceiling(count_encounters/length(unique(df.cohort.trunc$encounter_id))*100)," %")
              )
  df.conditions.summary[is.na(df.conditions.summary)] <- "NA"
  openxlsx:::addWorksheet(wb, "Stroke_ICD_Summary")
  openxlsx:::writeDataTable(wb = wb,x = df.conditions.summary,sheet = "Stroke_ICD_Summary", withFilter = FALSE)

#7. #################################################################################    
#Summary based on ICD count, Rank and encounters

ICD.rank <- df.cohort%>%
    group_by(encounter_id,rank_indicator)%>%
    summarise(icd_counts= length(unique(icd)))%>%
    group_by(icd_counts,rank_indicator)%>%summarise(encounters= length(unique(encounter_id)))

  ICD.rank[is.na(ICD.rank)] <- "NA"  
openxlsx:::addWorksheet(wb, "ICD_Rank_Summary")
openxlsx:::writeDataTable(wb = wb,x = ICD.rank,sheet = "ICD_Rank_Summary", withFilter = FALSE)

#8. ################################################################################# 
  #Monthly counts for different ICDS
  df.cohort$year_month <- substr(coalesce(as.character(df.cohort$admission_date),as.character(df.cohort$recorded_date)),1,7)
  monthly <- df.cohort%>%
    group_by(year_month)%>%
    summarise(count_unique_patient = length(unique(patient_id)), 
              count_unique_encounters = length(unique(encounter_id)),
              all_encounters = length(encounter_id),
              diag_i60_encounters = length(unique(encounter_id[which(icd_family == 'I60')])),
              diag_i61_encounters = length(unique(encounter_id[which(icd_family == 'I61')])),
              diag_i63_encounters = length(unique(encounter_id[which(icd_family == 'I63')]))
    ) 
  monthly[,c(2:7)] <- lapply(monthly[,c(2:7)], function(x) ifelse(x<5, "< 5", x))
  monthly[is.na(monthly)] <- "NA"
openxlsx:::addWorksheet(wb, "Monthly_Summary")
openxlsx:::writeDataTable(wb = wb,x = monthly,sheet = "Monthly_Summary", withFilter = FALSE)
}

#9. #################################################################################  
#Procedure Summary
if(nrow(df.procedure) > 0){
  
  #count for different procedures performed in case
  df.procedure.summary <- df.procedure %>%
    group_by(procedures = features) %>%
    summarise(count_encounters = length(unique(encounter_id))
              ,count_encounters = count_encounters  + ifelse(test = count_encounters>5,sample(-5:5, 1,replace = TRUE),sample(0:5, 1,replace = TRUE))
              ,percent_encounters = paste0(ceiling(count_encounters/length(unique(df.cohort.trunc$encounter_id))*100)," %"))
  df.procedure.summary[is.na(df.procedure.summary)] <- "NA"
  openxlsx:::addWorksheet(wb, "Different_Procedures")
  openxlsx:::writeDataTable(wb = wb,x = df.procedure.summary,sheet = "Different_Procedures", withFilter = FALSE)
}
#10. #################################################################################  

#Previous Condition Summary
if(nrow(df.conditions.previous) > 0){
  
  
  #previous comorbities summary
  df.conditions.previous.summary <- df.conditions.previous %>%
    group_by(comorbidities = features) %>%
    summarise(count_encounters = length(unique(encounter_id)) 
              ,count_encounters = count_encounters  + ifelse(test = count_encounters>5,sample(-5:5, 1,replace = TRUE),sample(0:5, 1,replace = TRUE))
              ,percent_encounters = paste0(ceiling(count_encounters/length(unique(df.cohort.trunc$encounter_id))*100)," %"))
  
  df.conditions.previous.summary[is.na(df.conditions.previous.summary)] <- "NA"
  
  openxlsx:::addWorksheet(wb, "Previous_Comorbidities")
  openxlsx:::writeDataTable(wb = wb,x = df.conditions.previous.summary,sheet = "Previous_Comorbidities", withFilter = FALSE)
}

#11. #################################################################################  
#observation summary
if(nrow(df.observation) > 0){
  df.observation.summary <- df.observation%>%
    group_by(loinc_code) %>%
    summarise(count_encounters = length(unique(encounter_id)) 
              ,count_encounters = count_encounters  + ifelse(test = count_encounters>5,sample(-5:5, 1,replace = TRUE),sample(0:5, 1,replace = TRUE))
              ,percent_encounters = paste0(ceiling(count_encounters/length(unique(df.cohort.trunc$encounter_id))*100)," %"))
  
  df.observation.summary[is.na(df.observation.summary)] <- "NA"
  openxlsx:::addWorksheet(wb, "Lab_Values")
  openxlsx:::writeDataTable(wb = wb,x = df.observation.summary,sheet = "Lab_Values", withFilter = FALSE)
}

#12. #################################################################################  
#medication summary
if(exists("df.medication") && nrow(df.medication) > 0){
  df.med.summary <- df.medstatement%>%
    group_by(code) %>%
    summarise(count_encounters = length(unique(encounter_id)) 
              ,count_encounters = count_encounters  + ifelse(test = count_encounters>5,sample(-5:5, 1,replace = TRUE),sample(0:5, 1,replace = TRUE))
              ,percent_encounters = paste0(ceiling(count_encounters/length(unique(df.cohort.trunc$encounter_id))*100)," %"))
  df.med.summary[is.na(df.med.summary)] <- "NA"
  openxlsx:::addWorksheet(wb, "Medication")
  openxlsx:::writeDataTable(wb = wb,x = df.med.summary,sheet = "Medication", withFilter = FALSE)
}

##################################################################################  
#export the summary
openxlsx:::saveWorkbook(wb, "Summary/Summary_Step1_MIRACUM_WESTORM.xlsx", overwrite = TRUE)

###logging run time##################################################################################  
runtime <- Sys.time() - start

con <- file("Summary/miracum_select.log")
write(paste0(
  "miracum_select.R finished at ", Sys.time(), ".\n",
  "Extracted ", length(unique(df.cohort$encounter_id)), " Encounters based on ", length(unique(df.cohort$patient_id)), " Patients.\n", 
  "R script execution took ", round(runtime, 2), " ", attr(runtime, "units"), "."
), file = con)
close(con)


