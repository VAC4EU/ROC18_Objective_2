##%######################################################%##
#                                                          #
####          APPLY GENERAL EXCLUSION CRITERIA          ####
####             TO CREATE STUDY POPULATION             ####
#                                                          #
##%######################################################%##

print('FLOWCHART')

# USE THE FUNCTION CREATEFLOWCHART TO SELECT THE SUBJECTS IN POPULATION

if (TEST){ 
  testname <- "test_03_T2_11_create_cohort_flowcharts"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- paste0(file.path(dirtest, testname, "g_output_program"), "/")
  dir.create(thisdiroutput, showWarnings = F)
  immune_diseases_in_this_step <- c("E_GRAVES_AESI")
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- diroutput
  immune_diseases_in_this_step <- immune_diseases_in_the_study
}

for (immdis in immune_diseases_in_this_step) {
  
  # Create flowchart for adults and save D4_study_population
  selection_criteria <- readRDS(paste0(thisdirinput, "D3_cohort_", immdis, ".rds"))
  
  listcriteria <- paste0(c("has_not_a_code_in_the_study_period_",
                           "exclude_because_exist_code_during_lookback_",
                           "exclude_because_exist_exclusion_criterion_during_lookback_"), immdis)
  
  selected_population <- CreateFlowChart(
    dataset = selection_criteria,
    listcriteria = listcriteria,
    flowchartname = paste0("Flowchart_exclusion_criteria_", immdis))
  
  fwrite(get(paste0("Flowchart_exclusion_criteria_", immdis)),
         paste0(thisdiroutput, "Flowchart_exclusion_criteria_", immdis, ".csv"))
  
  # selected_population <- selected_population[, .(person_id, birth_date, death_date, sex_at_instance_creation,
  #                                                spell_start_date, study_entry_date, study_exit_date)]
  # 
  # nameoutput <- paste0("D4_source_population", suffix[[subpop]])
  # assign(nameoutput, selected_population)
  # save(nameoutput, file = paste0(diroutput, nameoutput, ".RData"), list = nameoutput)
  # rm(list = nameoutput)
}
