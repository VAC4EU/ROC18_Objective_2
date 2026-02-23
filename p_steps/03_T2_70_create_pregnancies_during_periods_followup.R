############################################################################
#                                                          #
####   CREATE D3_pregnancy_while_in_followup_periods_in_cohort_{ImmDis} ####
# 
#                                                          #
############################################################################

# author: Rosa Gini
# 
# v 1.0.0  29 Sep 2024

#########################################

#########################################
# assign input and output directories

if (TEST){ 
  immdis <- "E_GRAVES_AESI"
  testname <- "test_03_T2_60_create_pregnancies_during_periods_followup"
  thisdirinput <- file.path(dirtest,testname)
  thisskip_pregnancy <- F
  thisdirpregnancy <- thisdirinput
  thisdiroutput <- file.path(dirtest,testname,"g_output_program")
  dir.create(thisdiroutput, showWarnings = F)
  immune_diseases_in_this_step <- c(immdis)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisskip_pregnancy <- skip_pregnancy
  thisdirpregnancy <- dirpregnancy
  immune_diseases_in_this_step <- immune_diseases_in_the_study
}

# load input datasets to be used by all diseases

if (thisskip_pregnancy) {
  for (immdis in immune_diseases_in_this_step){ 
    # create empty datasets? or, take into account this possibility in the next step?
    }
  }else{
  
  load(file.path(thisdirpregnancy,"D3_pregnancy_final.RData"))
    
  pregnancies <- D3_pregnancy_final[, .(pregnancy_id,person_id,pregnancy_start_date,pregnancy_end_date)]
  
  pregnancies[, start_period_in_this_pregnancy := pregnancy_start_date]
  pregnancies[, end_period_in_this_pregnancy := pregnancy_end_date]
  rm(D3_pregnancy_final)
  
  for (immdis in immune_diseases_in_this_step){
    
    periods_followup <- readRDS(file.path(thisdirinput,paste0("D3_followup_periods_in_cohort_",immdis,".rds")))
    
    tokeep <- c("person_id",paste0("number_of_period_",immdis), paste0("start_period_",immdis,"_d"),paste0("end_period_",immdis,"_d"),paste0("cause_end_period_",immdis))

    periods_followup <- periods_followup[,..tokeep]
    
    setnames(periods_followup, c(paste0("number_of_period_",immdis), paste0("start_period_",immdis,"_d"),paste0("end_period_",immdis,"_d"),paste0("cause_end_period_",immdis)), c("number_of_period", "start_period_d","end_period_d","cause_end_period"))
    
    periods_followup[, start_followup_period := start_period_d]
    periods_followup[, end_followup_period := end_period_d]
    
    
    processing <- GenerateTDDataset(datasets = list(pregnancies,periods_followup),
                                      UoO_vars = c("person_id","person_id"),
                                      start_d_vars = c("start_period_in_this_pregnancy","start_period_d"),
                                      end_d_vars = c("end_period_in_this_pregnancy","end_period_d"),
                                      keep_auxiliary_variables = F,
                                      TD_variables = list(list("pregnancy_id","pregnancy_start_date","pregnancy_end_date"),list("number_of_period","cause_end_period","start_followup_period", "end_followup_period")),
                                      keep_periods_observed_by = "both", TD_variables_with_definite_value_until_unobserved = c("number_of_period","cause_end_period")
    )
    
    # number_of_period_in_this_pregnancy 
    
    setorderv(processing, c("pregnancy_id", "start_period_in_this_pregnancy"))
    processing[, number_of_period_in_this_pregnancy := seq_len(.N), by= "pregnancy_id"]
    
  setnames(processing, c("number_of_period", "start_followup_period",	"end_followup_period","cause_end_period", "start_period_in_this_pregnancy", "end_period_in_this_pregnancy", "number_of_period_in_this_pregnancy"),c(paste0("number_of_period_",immdis), paste0("start_period_",immdis,"_d"),	paste0("end_period_",immdis,"_d"), paste0("cause_end_period_",immdis), paste0("start_period_in_this_pregnancy_",immdis,"_d"), paste0("end_period_in_this_pregnancy_",immdis,"_d"), paste0("number_of_period_in_this_pregnancy_",immdis)))
    
    tokeep <- c("pregnancy_id", "pregnancy_start_date", "pregnancy_end_date", "person_id", paste0("number_of_period_",immdis), paste0("start_period_",immdis,"_d"),	paste0("end_period_",immdis,"_d"), paste0("cause_end_period_",immdis), paste0("start_period_in_this_pregnancy_",immdis,"_d"), paste0("end_period_in_this_pregnancy_",immdis,"_d"), paste0("number_of_period_in_this_pregnancy_",immdis))
    
    processing[, ..tokeep]
    
    
    
    
    ########################################
    # save
    
    outputfile <- processing
    nameoutput <- paste0("D3_pregnancy_while_in_followup_periods_in_cohort_",immdis)
    assign(nameoutput, outputfile)
    saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
      
  }

  }
