
if (TEST){ 
  testname <- "test_03_T2_50_create_periods_followup"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdirinput2 <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- paste0(file.path(dirtest, testname, "g_output_program"), "/")
  dir.create(thisdiroutput, showWarnings = F)
  immune_diseases_in_this_step <- c("E_GRAVES_AESI")
}else{
  thisdirinput <- dirtemp
  thisdirinput2 <- dirTD
  thisdiroutput <- dirtablesubpop[[subpop]]
  immune_diseases_in_this_step <- immune_diseases_in_the_study
  # immdis <- "E_GRAVES_AESI"
}

base_table3 <- data.table::data.table(ds = thisdatasource)

for (immdis in immune_diseases_in_this_step) {
  
  # load input datasets to be used by all diseases
  cohort <- readRDS(paste0(thisdirinput, paste0("D3_cohort_", immdis, ".rds")))
  
  simple_names <- c("has_not_a_code_in_the_study_period", "exclude_because_exist_code_during_lookback",
                    "exclude_because_exist_exclusion_criterion_during_lookback", "enter_cohort", 
                    "cohort_entry_date", "cause_for_not_entering_followup", 
                    "entering_follow_up_postponed", "entering_follow_up", "start_follow_up")
  setnames(cohort, paste(simple_names, immdis, sep = "_"), simple_names)
  
  # Remove persons not entering the cohort
  cohort <- cohort[has_not_a_code_in_the_study_period == 0 & exclude_because_exist_code_during_lookback == 0 &
                     exclude_because_exist_exclusion_criterion_during_lookback == 0, ]
  cohort <- cohort[, .(person_id, cohort_entry_date)]
  
  table3 <- copy(base_table3)
  table3[, n_1 := nrow(cohort)]
  
  COV_variables_combined <- c(COV_variables, DP_variables, "PREGNANCY")
  for (i in seq_along(COV_variables_combined)) {
    
    if (table3[, n_1] == 0) {
      new_col_name <- paste("n_Cov", immdis, COV_variables_combined[[i]], sep = "_")
      table3[, (new_col_name) := 0]
      new_col_name <- paste("p_Cov", immdis, COV_variables_combined[[i]], sep = "_")
      table3[, (new_col_name) := 0]
      
      next
    }
    
    covariate_df <- readRDS(file = paste0(thisdirinput2, paste0("D3_TD_", COV_variables_combined[[i]]), ".rds"))
    cohort_cov <- covariate_df[cohort, .(person_id, date = x.date, value_of_variable), on = .(person_id, date < cohort_entry_date)]
    cohort_cov <- cohort_cov[cohort_cov[, .I[date == max(date)], by = "person_id"]$V1]
    
    new_col_name <- paste("n_Cov", immdis, COV_variables_combined[[i]], sep = "_")
    has_cov <- cohort_cov[, sum(value_of_variable, na.rm = T), ]
    table3[, (new_col_name) := has_cov]
    new_col_name <- paste("p_Cov", immdis, COV_variables_combined[[i]], sep = "_")
    table3[, (new_col_name) := round(100 * has_cov / n_1, 1)]
  }
  
  listvar <- grep("^n_Cov", names(table3), value = TRUE)
  listvar_p <- grep("^p_Cov", names(table3), value = TRUE)
  
  if (thisdatasource %in% datasources_stricter_masking) {
    table3 <- range_values_against_recalculation(table3, "n_1")
    table3 <- mask_variables_per_threshold(table3, listvar, threshold[[thisdatasource]], listvar_p)
  } else {
    table3 <- mask_variables_per_threshold(table3, listvar, threshold[[thisdatasource]])
  }
  
  table3[, threshold := threshold[[thisdatasource]]]
  
  fwrite(table3, paste0(thisdiroutput, "D5_Table_3_Covariates_", immdis, ".csv"))
}
