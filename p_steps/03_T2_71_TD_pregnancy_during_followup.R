############################################################################
#                                                          #
####   CREATE 03_T2_61_TD_pregnancy_during_followup_{ImmDis} ####
# 
#                                                          #
############################################################################

# author: Rosa Gini
# 
# v 1.0.0  30 Sep 2024

#########################################

#########################################
# assign input and output directories

if (TEST){ 
  immdis <- "E_GRAVES_AESI"
  testname <- "test_03_T2_61_TD_pregnancy_during_followup"
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
  
  for (immdis in immune_diseases_in_this_step){
    
    TD <- readRDS(file.path(thisdirinput,paste0("D3_pregnancy_while_in_followup_periods_in_cohort_",immdis,".rds")
))
    setnames(TD, c( paste0("start_period_in_this_pregnancy_",immdis, "_d"), paste0("end_period_in_this_pregnancy_",immdis, "_d")), c("start_period_in_this_pregnancy", "end_period_in_this_pregnancy")  )
    
    TD <- TD[, .(person_id, start_period_in_this_pregnancy, end_period_in_this_pregnancy)]
    
    setnames(TD, c("start_period_in_this_pregnancy", "end_period_in_this_pregnancy"), c("start_record_d", "end_record_d"))
    
    TD[, (paste0("pregnancy_",immdis)) := 1]
    

    ########################################
    # save
    
    outputfile <- TD
    nameoutput <- paste0("D3_TD_pregnancy_during_followup_",immdis)
    assign(nameoutput, outputfile)
    saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
    
  #  # rm(components_flare)
  
      
  }

  }
