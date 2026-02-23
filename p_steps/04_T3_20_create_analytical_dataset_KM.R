############################################################
#                                                          #
####   CREATE D4_analytical_dataset_KM_{ImmDis} ####
# 
#                                                          #
############################################################

# author: Rosa Gini
# 
# v 1.0.0  XXX

#########################################

#########################################
# assign input and output directories

if (TEST){ 
  immdis <- "E_GRAVES_AESI"
  testname <- "test_04_T3_20_create_analytical_dataset_KM"
  thisdirinput <- file.path(dirtest,testname)
  thisdirconceptsets <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output_program")
  dir.create(thisdiroutput, showWarnings = F)
  immune_diseases_in_this_step <- c(immdis)
}else{
  thisdirinput <- substr(dirtemp, 1, nchar(dirtemp)-1)
  thisdiroutput <- substr(dirtemp, 1, nchar(dirtemp)-1)
  thisdirconceptsets <- substr(dirconceptsets, 1, nchar(dirconceptsets)-1)
  immune_diseases_in_this_step <- immune_diseases_in_the_study
}

# load input datasets to be used by all diseases: none

for (immdis in immune_diseases_in_this_step){
  
  load(file.path(thisdirinput,paste0("D3_PERSONS.RData")))
  persons <- D3_PERSONS
  persons <- persons[,.(person_id,sex_at_instance_creation,birth_date)]
  
  fup_periods <- readRDS(file.path(thisdirinput,paste0("D3_followup_periods_in_cohort_",immdis,".rds")))
  
  # flares <- readRDS(file.path(thisdirinput,paste0("D3_followup",immdis,".rds")))
  
    outputfile <- fup_periods[get(paste0("number_of_period_",immdis)) == 1,]
    
    outputfile <- merge(outputfile,persons,by = "person_id", all.x = T)
    
    outputfile[, (paste0("flare_",immdis)) := fifelse(get(paste0("cause_end_period_",immdis)) == 5, 1, 0)]
    
    outputfile[, (paste0("age_at_cohort_entry_date_",immdis)):= age_fast(birth_date, get(paste0("cohort_entry_date_",immdis))) ]
    
    outputfile[, (paste0("days_",immdis)):= as.integer(get(paste0("end_period_",immdis,"_d")) - get(paste0("start_period_",immdis,"_d")) + 1 ) ]
    

    tokeep <- c("person_id","sex_at_instance_creation", paste0("age_at_cohort_entry_date_",immdis), paste0("start_follow_up_",immdis), paste0("start_period_",immdis,"_d"), paste0("end_period_",immdis,"_d"), paste0("days_",immdis), paste0("cause_end_period_",immdis), paste0("flare_",immdis))
    
    outputfile <- outputfile[, ..tokeep]
  
  ########################################
  # save

  # outputfile <- TD
  nameoutput <- paste0("D4_analytical_dataset_KM_",immdis)
  assign(nameoutput, outputfile)
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
  rm(fup_periods,outputfile)
}

