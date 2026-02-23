############################################################################
#                                                          #
####   CREATE D3_followup_periods_in_cohort_with_pregnancy_{ImmDis} ####
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
  testname <- "test_03_T2_70_create_periods_followup_with_pregnancies"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisskip_pregnancy <- F
  thisdirpregnancy <- thisdirinput
  thisdiroutput <- paste0(file.path(dirtest, testname, "g_output_program"), "/")
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


for (immdis in immune_diseases_in_this_step){
    
  if (thisskip_pregnancy) {
      TD <- readRDS(paste0(thisdirinput, "D3_followup_periods_in_cohort_", immdis, ".rds")
      )
       TD[, (paste0("pregnancy_",immdis)) := 0]
      
    }else{
      
    periods_followup <- readRDS(paste0(thisdirinput, "D3_followup_periods_in_cohort_",immdis,".rds"))
    
    TDpreg <- readRDS(paste0(thisdirinput, "D3_TD_pregnancy_during_followup_", immdis, ".rds")
)
    setnames(TDpreg,paste0("pregnancy_", immdis),"pregnancy")
    
    TD <- processing <- GenerateTDDataset(datasets = list(periods_followup,TDpreg),
                        UoO_vars = c("person_id","person_id"),
                        start_d_vars = c(paste0("start_period_", immdis,"_d"),"start_record_d"),
                        end_d_vars = c(paste0("end_period_", immdis,"_d"),"end_record_d"),
                        keep_auxiliary_variables = F,
                        TD_variables = list(
                          list(paste0("number_of_period_", immdis), paste0("number_of_period_at_risk_flare_", immdis), paste0("cause_end_period_", immdis)),
                          list("pregnancy")
                               ),
                        keep_periods_observed_by = "first", 
                        default_value_for_unobserved = list(
                          # paste0("pregnancy_", immdis) = 0
                          pregnancy = 0
                        )
                        )
    setnames(TD,"pregnancy",paste0("pregnancy_", immdis))
    
    rm(TDpreg,periods_followup)
    
  }    
    ########################################
    # save
    
    outputfile <- TD
    nameoutput <- paste0("D3_followup_periods_in_cohort_with_pregnancy_",immdis)
    assign(nameoutput, outputfile)
    saveRDS(outputfile, file = paste0(thisdiroutput, nameoutput, ".rds"))
    rm(TD, outputfile)
  }
