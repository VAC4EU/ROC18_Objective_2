############################################################
#                                                          #
####   CREATE D4_component_dataset_{ImmDis} ####
# 
#                                                          #
############################################################

# author: Rosa Gini
# 
# v 1.0.0  26 Sep 2024

#########################################

#########################################
# assign input and output directories

if (TEST){ 
  immdis <- "E_GRAVES_AESI"
  testname <- "test_04_T3_30_create_components_dataset"
  thisdirinput <- file.path(dirtest,testname)
  thisdirconceptsets <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output_program")
  dir.create(thisdiroutput, showWarnings = F)
  immune_diseases_in_this_step <- c(immdis)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirconceptsets <- dirconceptsets
  immune_diseases_in_this_step <- immune_diseases_in_the_study
}

# load input datasets to be used by all diseases: none

for (immdis in immune_diseases_in_this_step){
  
  processing <- readRDS(file.path(thisdirinput,paste0("D3_components_flare_TD_",immdis,".rds")))
  
  setnames(processing, paste0("start_follow_up_",immdis), "start_follow_up")
  
  processing <- processing[,.(person_id, start_follow_up, date_flare_Diagnoses, date_flare_Medications, date_flare_Procedures, date_flare_Emergency,date_flare_Hosp,date_flare_Flare)]
  
  cols_to_change <- names(Filter(is.Date, processing))
  processing <- processing[, (cols_to_change) := lapply(.SD, data.table:::as.Date.IDate), .SDcols = cols_to_change]
  
  cols_to_change <- colnames(processing)[grepl("^date_flare_", colnames(processing))]
  processing <- processing[, (cols_to_change) := lapply(.SD, as.Date), .SDcols = cols_to_change]
  
  for (bblock in c("Diagnoses", "Medications", "Procedures", "Emergency", "Hosp","Flare")) {
    processing[,(paste0("diff_",bblock)) := as.integer(get(paste0("date_flare_",bblock)) - start_follow_up) + 1]
    processing[,(paste0("is_flare_",bblock)) := fifelse(!is.na(get(paste0("diff_",bblock))) & get(paste0("diff_",bblock)) <= 365 & get(paste0("diff_",bblock)) > 0,1,0)]
  }
  
  processing <- processing[, lapply(.SD,max, na.rm = T), by = c("person_id", "start_follow_up"),  .SDcols = c("is_flare_Diagnoses", "is_flare_Medications", "is_flare_Procedures", "is_flare_Emergency", "is_flare_Hosp", "is_flare_Flare") ]
  
  processing <- processing[, .N, by = c("is_flare_Diagnoses", "is_flare_Medications", "is_flare_Procedures", "is_flare_Emergency", "is_flare_Hosp", "is_flare_Flare") ]

  ########################################
  # save

  outputfile <- processing
  nameoutput <- paste0("D4_component_dataset_",immdis)
  assign(nameoutput, outputfile)
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
  rm(processing)


}
 
