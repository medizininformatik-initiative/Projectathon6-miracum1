### Preparation

#load/install a packages
source("install_R_packages.R")

#create directories
if(!dir.exists("Ergebnisse")){dir.create("Ergebnisse")}
if(!dir.exists("Summary")){dir.create("Summary")}
if(!dir.exists("errors")){dir.create("errors")}
if(!dir.exists("Bundles")){dir.create("Bundles")}


#read  config
if(file.exists("config.yml")){
  conf <- config::get(file = "config.yml")
}else{
  conf <- config::get(file = "config_default.yml")
}



brackets = c("[", "]")
sep = " || "
############Data extraction#############################
##extract the condition resources of the list of stroke ICD's greather than 2015-01-01 and extract the associated patient, encounter, condition and procedure resource
#configure the fhir search url
encounter_request <- fhir_url(url = conf$serverbase, 
                              resource = "Encounter", 
                              parameters = c("date" = "ge2015-01-01",
                                             "_has:Condition:encounter:code"="I60.0,I60.1,I60.2,I60.3,I60.4,I60.5,I60.6,I60.7,I60.8,I60.9,I61.0,I61.1,I61.2,I61.3,I61.4,I61.5,I61.6,I61.8,I61.9,I63.0,I63.1,I63.2,I63.3,I63.4,I63.5,I63.6,I63.8,I63.9,I67.80!",
                                             "_include" = "Encounter:patient",
                                             "_revinclude"="Condition:encounter",
                                             "_revinclude"="Procedure:encounter",
                                             "_parameter_count" = "500"     # To speed up the download process
                              ))
#"_profile" = "https://www.medizininformatik-initiative.de/fhir/core/modul-labor/StructureDefinition/ObservationLab"))

#download the bundles
enc_bundles <- fhir_search(request = encounter_request, username = conf$user, password = conf$password, verbose = 1,log_errors = "errors/encounter_error.xml")
#fhir_save(bundles = enc_bundles, directory = "Bundles/Encounters")





#############################
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
                                              condition_id ="diagnosis/condition/reference",
                                              patient_id = "subject/reference",
                                              rank = "diagnosis/rank",
                                              discharge_reason = "hospitalization/dischargeDisposition/coding/code"),
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

procedure <- fhir_table_description(resource = "Procedure",
                                    cols = c(procedure_id = "id",
                                             performed_date= "performedDateTime",
                                             ops = "code/coding/code",
                                             system         = "code/coding/system",
                                             encounter_id = "encounter/reference",
                                             patient_id     = "subject/reference"),
                                    style = fhir_style(sep=sep,
                                                       brackets = brackets,
                                                       rm_empty_cols = FALSE)
)


#flatten the resource

enc_tables <- fhir_crack(enc_bundles, 
                         design = fhir_design(enc = encounters, pat = patients, con = condition, proc= procedure),
                         data.table = TRUE)




if(nrow(enc_tables$enc) == 0){
  write("Could not find any encounter resource in the server for the required stroke condition. Query Stopped.", file ="errors/error_message.txt")
  stop("No Stroke encounters found - aborting.")
}

if(nrow(enc_tables$pat) == 0){
  write("Could not find any patient resource in the server for the required stroke condition. Query Stopped.", file ="errors/error_message.txt")
  stop("No Patients for stroke condition found - aborting.")
}


###############extract and process patient resource##############################
df.patients <- enc_tables$pat
df.patients <- fhir_rm_indices(df.patients, brackets = brackets )



################extract encounter resource###############

df.encounters <- enc_tables$enc
df.encounters <- fhir_melt(df.encounters,
                           columns = c('encounter_id','admission_date', 'condition_id','patient_id' ,'rank','discharge_reason'),
                           brackets = brackets, sep = sep, all_columns = TRUE)
df.encounters <- fhir_rm_indices(df.encounters, brackets = brackets )

df.encounters <- zoo:::na.locf(df.encounters, na.rm = F)
df.encounters$condition_id <- sub("Condition/", "", df.encounters$condition_id)
df.encounters$patient_id <- sub("Patient/", "", df.encounters$patient_id)


####################extract the condition resources##################

df.conditions <- enc_tables$con
df.conditions <- fhir_rm_indices(df.conditions, brackets = brackets )
df.conditions$encounter_id <- sub("Encounter/", "", df.conditions$encounter_id)
df.conditions$patient_id <- sub("Patient/", "", df.conditions$patient_id)

icd_codes <- c('I60.0','I60.1','I60.2','I60.3','I60.4','I60.5','I60.6','I60.7','I60.8','I60.9'
               ,'I61.0','I61.1','I61.2',  'I61.3','I61.4','I61.5','I61.6','I61.8','I61.9'
               ,'I63.0','I63.1','I63.2','I63.3','I63.4','I63.5','I63.6','I63.8','I63.9','I67.80!')

df.conditions <- df.conditions[c(which(df.conditions$icd %in% icd_codes) )]




####################extract the procedure resources##################
df.procedure <- enc_tables$proc
if(nrow(df.procedure) > 0){
  df.procedure <- fhir_rm_indices(df.procedure, brackets = brackets )
  df.procedure$encounter_id <- sub("Encounter/", "", df.procedure$encounter_id)
  df.procedure$patient_id <- sub("Patient/", "", df.procedure$patient_id)
  
  
  
  #Filter resources with needed ops code 
  ops_codes <- c("8-020.8|8-020.D|8-980|8-981|8-981.2|8-981.20|8-981.21|8-981.22|8-981.23|8-981.3|5-025|5-026|5-026.4|8-83B.8|8-84B.0|8-84B.2|8-84B.3|8-84B.4|8-84B.5|8-706|8-713.0|8-980|8-98F|8-98B")
  df.procedure <- df.procedure %>% filter(str_detect(ops, ops_codes))
  
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
    summarise(ops = paste((ops), collapse = ','))
  
  df.procedure.wide <- pivot_wider(data = df.procedure.wide,
                                   names_from = features,
                                   values_from = ops,
                                   id_cols = c(encounter_id,patient_id))
  
  
  
} #endif
#######################################################################################################################


#######################################################################################################################
#extract observation resource for the required ids and loinc codes
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


observation_list  <- lapply(list, function(x){
  
  ids <- paste(x, collapse = ",")
  #777-3,6301-6,3173-2,2160-0,2089-1,2085-9,7799-0,4548-4,2345-7,2093-3
  obs_request <- fhir_url(url = conf$serverbase,
                          resource = "Observation",
                          parameters = c(encounter = ids
                                         ,"code" = "777-3,6301-6,3173-2,2160-0,2089-1,2085-9,7799-0,4548-4,2345-7,2093-3,74201-5"))
  
  obs_bundles <- fhir_search(obs_request,
                             username = conf$username,
                             password = conf$password,
                             log_errors = "errors/Observations_error.xml")
  
})




#bring observation results together, save and flatten
observation_bundles <- fhircrackr:::fhir_bundle_list(unlist(observation_list, recursive = F))
#fhir_save(bundles = observation_bundles, directory = "Bundles/observations")

observation <- fhir_table_description(resource = "Observation",
                                      cols = c(observation_id = "id",
                                               effective_date= "effectiveDateTime",
                                               loinc_code = "code/coding/code",
                                               system         = "code/coding/system",
                                               display = "code/coding/display",
                                               value =  "valueQuantity/value",
                                               unit =  "valueQuantity/unit",
                                               encounter_id = "encounter/reference",
                                               patient_id     = "subject/reference"),
                                      style = fhir_style(sep=sep,
                                                         brackets = brackets,
                                                         rm_empty_cols = FALSE)
)


obs_table <- fhir_crack(observation_bundles,design = fhir_design(obs = observation))
df.observation <- obs_table$obs

#process observations_raw resources
if(nrow(df.observation) > 0){
  df.observation <- fhir_rm_indices(df.observation, brackets = brackets )
  df.observation$encounter_id <- sub("Encounter/", "", df.observation$encounter_id)
  df.observation$patient_id <- sub("Patient/", "", df.observation$patient_id)  
  
  #df.observation.wide <- pivot_wider()
  
}

#######################################################################################################################
#extract medicationstatement resource for the required encounter ids and atc codes
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
  
}) # lapply()


medstat_bundles <- fhircrackr:::fhir_bundle_list(unlist(medstat_list, recursive = F))
#fhir_save(bundles = observation_bundles, directory = "Bundles/observations")


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

medstat_table <- fhir_crack(medstat_bundles,design = fhir_design(medstat = medstat))
df.medstatement <- medstat_table$medstat

if(nrow(df.medstatement)>0){
  #process Medication statement  resources
  df.medstatement <- fhir_rm_indices(df.medstatement, brackets = brackets )
  df.medstatement$encounter_id <-vsub("Encounter/", "", df.medstatement$encounter_id)
  df.medstatement$patient_id <-vsub("Patient/", "", df.medstatement$patient_id) 
  df.medstatement$medication_id <-vsub("Medication/", "", df.medstatement$medication_id) 
  
  
  
  ###extract the actual medication using the IDs
  medication_ids <- unique(df.medstatement$medication_id)
  
  
  med_request <- fhir_url(url = conf$serverbase,
                          resource = "Medication",
                          parameters = c("_id" = unique(df.medstatement$medication_id))
                          )
  
  med_bundle <- fhir_search(med_request,
                            username = conf$username,
                            password = conf$password,
                            log_errors = "errors/medication_error.xml")
  
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
  
  med_table <- fhir_crack(med_bundle,design = fhir_design(med = medication))
  
  
  
  
  
  
  df.medication <- med_table$med
  if(nrow(df.medication >0 )){
    
    #process Medication statement  resources
    df.medication <- fhir_rm_indices(df.medication, brackets = brackets )
    df.medstatement <- left_join(df.medstatement,df.medication,"medication_id")
  } # endif
  
} #endif

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
  
}) # lapply


condition_bundles <- fhircrackr:::fhir_bundle_list(unlist(condition_list, recursive = F))
fhir_save(bundles = observation_bundles, directory = "Bundles/conditions")

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

cond_table <- fhir_crack(condition_bundles,design = fhir_design(con = condition))
df.conditions.previous <- cond_table$con

if(nrow(df.conditions.previous) > 0){
  #process observations_raw resources
  df.conditions.previous <- fhir_rm_indices(df.conditions.previous, brackets = brackets)
  df.conditions.previous$encounter_id <-sub("Encounter/", "", df.conditions.previous$encounter_id)
  df.conditions.previous$patient_id <-sub("Patient/", "", df.conditions.previous$patient_id) 
  df.conditions.previous <- df.conditions.previous[-c(which(df.conditions.previous$encounter_id %in% encouter_ids)), ]
  
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
      group_by(patient_id,encounter_id,features)%>%
      summarise(icd = paste((icd), collapse = ','))
    
    df.conditions.previous.wide <- pivot_wider(data = df.conditions.previous.wide
                                               ,names_from = features
                                               ,values_from = icd
                                               ,id_cols = c(encounter_id,patient_id))
  }
  
} # endif


####################################join all the resources ##################################################
df.cohort <- left_join(df.conditions,df.encounters[,c("condition_id","admission_date","rank","discharge_reason")],"condition_id")
df.cohort <- left_join(df.cohort,df.patients,"patient_id")

df.cohort <- df.cohort[,c("patient_id","birthdate","gender","patient_zip"
                          ,"encounter_id","admission_date","icd","system"
                          ,"recorded_date","rank")]


df.cohort.agg <- df.cohort%>%
  group_by(patient_id,encounter_id)%>%
  summarise(birthdate = unique(birthdate),
            gender = unique(gender),
            patient_zip = unique(patient_zip),
            admission_date= unique(admission_date),
            icd = paste((icd), collapse = '/'),
            recorded_date = paste((recorded_date), collapse = '/'),
            rank = paste((rank), collapse = '/')
            ) 

if(exists("df.procedure.wide")){
  df.cohort <- left_join(df.cohort,subset(df.procedure.wide,select = -c(patient_id)),"encounter_id")
}

if(exists("df.conditions.previous.wide")){
  df.cohort <- left_join(df.cohort,subset(df.conditions.previous.wide,select = -c(encounter_id)),"patient_id")
}


###Export actual data 
if(!dir.exists("Ergebnisse")){dir.create("Ergebnisse")}
write.csv2(df.cohort, paste0("Ergebnisse/Kohorte.csv"))
write.csv2(df.observation, paste0("Ergebnisse/Observations.csv"))
write.csv2(df.medstatement, paste0("Ergebnisse/Medications.csv"))
#################################################################################################

###generate summary data and export####
#cohort summary
if(nrow(df.cohort) > 0){
  df.cohort.trunc <- df.cohort.agg
  df.cohort.trunc$year_quarter <-  ifelse(!is.na(df.cohort.trunc$admission_date), 
                                          as.character(zoo:::as.yearqtr(df.cohort.trunc$admission_date, format = "%Y-%m-%d")), 
                                          as.character(zoo:::as.yearqtr(df.cohort.trunc$recorded_date, format = "%Y-%m-%d"))
                                          )
  df.cohort.trunc[df.cohort.trunc=="NA"] = NA
  df.cohort.trunc.summary <- df.cohort.trunc %>%
    group_by(year_quarter) %>%
    summarise_all(funs(sum(!is.na(.))))
  write.csv2(df.cohort.trunc.summary, paste0("Summary/Cohort_Summary.csv"))
  
  df.conditions.summary <- df.conditions %>%
    group_by(icd) %>%
    summarise(count_encounters = length(unique(encounter_id)))
  write.csv2(df.conditions.summary, paste0("Summary/StrokeDiagnosis_Summary.csv"))
}

#Procedure Summary
if(nrow(df.procedure) > 0){
  df.procedure.summary <- df.procedure %>%
    group_by(features) %>%
    summarise(count_encounters = length(unique(encounter_id)))
  write.csv2(df.procedure.summary, paste0("Summary/Procedure_Summary.csv"))
}


#Previous Condition Summary
if(nrow(df.conditions.previous) > 0){
  df.conditions.previous.summary <- df.conditions.previous %>%
    group_by(features) %>%
    summarise(count_encounters = length(unique(encounter_id)))
  write.csv2(df.conditions.previous.summary, paste0("Summary/history_comorbidities_Summary.csv"))
  
  
}


#observation summary
if(nrow(df.observation) > 0){
  df.observation.summary <- df.observation%>%
    group_by(loinc_code) %>%
    summarise(count_encounters = length(unique(encounter_id)))
  write.csv2(df.observation.summary, paste0("Summary/Observation_Summary.csv"))
}


#medication summary
if(nrow(df.medstatement) > 0){
  df.med.summary <- df.medstatement%>%
    group_by(code) %>%
    summarise(count_encounters = length(unique(encounter_id)))
  write.csv2(df.med.summary, paste0("Summary/Medication_Summary.csv"))
}






