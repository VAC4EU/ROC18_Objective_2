
if (TEST){ 
  testname <- "05_T4_10_Table_1_Attrition"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdirinput_2 <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- paste0(file.path(dirtest, testname, "g_output_program"), "/")
  dir.create(thisdiroutput, showWarnings = F)
  immune_diseases_in_this_step <- c("E_GRAVES_AESI")
}else{
  thisdirinput <- diroutput
  thisdirinput_2 <- dirtemp
  thisdiroutput <- dirtablesubpop[[subpop]]
  immune_diseases_in_this_step <- immune_diseases_in_the_study
}

# Table 1

print("Now creating: Table 1")
base_table1 <- data.table::data.table(ds = thisdatasource)

flow_source <- fread(paste0(thisdirinput, "Flowchart_exclusion_criteria.csv"))

# excl_criteria <- c("sex_or_birth_date_is_not_defined", "birth_date_absurd", "partial_date_of_death", "no_spells",
#                    "all_spells_start_after_ending", "no_spell_longer_than_365_days",
#                    "no_spell_overlapping_the_study_period")

sex_birth_death_entry_exit_missing <- check_columns_exist(flow_source, c("sex_or_birth_date_is_not_defined", "birth_date_absurd",
                                                                         "partial_date_of_death", "no_spells", "all_spells_start_after_ending", "no_spell_longer_than_365_days"))
death_exit_before_2019 <- check_columns_exist(flow_source, c("no_spell_overlapping_the_study_period"))

flow_source[, excl_criteria := names(flow_source)[apply(flow_source, 1, which.min)]]
which_all_1 <- apply(flow_source[, .SD, .SDcols = c(sex_birth_death_entry_exit_missing, death_exit_before_2019)], 1,
                     function(x) all(as.logical(x)))
flow_source[which_all_1, excl_criteria := "Final study population"]

base_table1[, n_1 := flow_source[, sum(N)]]

base_table1[, n_2a := flow_source[excl_criteria %in% check_columns_exist(flow_source, c("sex_or_birth_date_is_not_defined")), sum(N)]]
base_table1[, p_2a := round(100 * n_2a / n_1, 1)]

base_table1[, n_2b := flow_source[excl_criteria %in% check_columns_exist(flow_source, c("birth_date_absurd")), sum(N)]]
base_table1[, p_2b := round(100 * n_2b / n_1, 1)]

base_table1[, n_2c := flow_source[excl_criteria %in% check_columns_exist(flow_source, c("partial_date_of_death")), sum(N)]]
base_table1[, p_2c := round(100 * n_2c / n_1, 1)]

base_table1[, n_2d := flow_source[excl_criteria %in% check_columns_exist(flow_source, c("no_spells")), sum(N)]]
base_table1[, p_2d := round(100 * n_2d / n_1, 1)]

base_table1[, n_2e := flow_source[excl_criteria %in% check_columns_exist(flow_source, c("all_spells_start_after_ending")), sum(N)]]
base_table1[, p_2e := round(100 * n_2e / n_1, 1)]

base_table1[, n_2f := flow_source[excl_criteria %in% check_columns_exist(flow_source, c("no_spell_longer_than_365_days")), sum(N)]]
base_table1[, p_2f := round(100 * n_2f / n_1, 1)]

base_table1[, n_3 := flow_source[excl_criteria %in% death_exit_before_2019, sum(N)]]
base_table1[, p_3 := round(100 * n_3 / n_1, 1)]

for (immdis in immune_diseases_in_this_step) {
  flow_source_cohort <- fread(paste0(thisdirinput, "Flowchart_exclusion_criteria_", immdis, ".csv"))
  
  no_code_study <- check_columns_exist(flow_source_cohort, c("has_not_a_code_in_the_study_period"))
  code_lookback <- check_columns_exist(flow_source_cohort, c("exclude_because_exist_code_during_lookback"))
  excl_lookback <- check_columns_exist(flow_source_cohort, c("exclude_because_exist_exclusion_criterion_during_lookback"))
  
  flow_source_cohort[, excl_criteria := names(flow_source_cohort)[apply(flow_source_cohort, 1, which.min)]]
  which_all_1 <- apply(flow_source_cohort[, .SD, .SDcols = c(no_code_study, code_lookback, excl_lookback)], 1,
                       function(x) all(as.logical(x)))
  flow_source_cohort[which_all_1, excl_criteria := "Final study population"]
  
  table1_coh <- data.table::copy(base_table1)
  
  cohort_pop <- readRDS(paste0(thisdirinput_2, "D3_cohort_", immdis, ".rds"))
  
  table1_coh[, n_4 := cohort_pop[, .N]]
  table1_coh[, p_4 := round(100 * n_4 / n_1, 1)]
  
  table1_coh[, n_5 := flow_source_cohort[excl_criteria %in% code_lookback, sum(N)]]
  table1_coh[, p_5 := round(100 * n_5 / n_1, 1)]
  
  table1_coh[, n_6 := flow_source_cohort[excl_criteria %in% excl_lookback, sum(N)]]
  table1_coh[, p_6 := round(100 * n_6 / n_1, 1)]
  
  table1_coh[, n_7 := flow_source_cohort[excl_criteria == "Final study population", N]]
  table1_coh[, p_7 := round(100 * n_7 / n_1, 1)]
  
  cohort_pop <- cohort_pop[eval(parse(text = paste0(paste0(c("has_not_a_code_in_the_study_period_",
                                                             "exclude_because_exist_code_during_lookback_",
                                                             "exclude_because_exist_exclusion_criterion_during_lookback_"), immdis)
                                                    , " == 0", collapse = " & ")))]
  
  cause_no_followup <- paste0("cause_for_not_entering_followup_", immdis)
  
  table1_coh[, n_8 := cohort_pop[eval(parse(text = paste0(cause_no_followup, " == 2"))), nrow(.SD), .SDcols = cause_no_followup]]
  table1_coh[, p_8 := round(100 * n_8 / n_1, 1)]
  
  table1_coh[, n_9 := cohort_pop[eval(parse(text = paste0(cause_no_followup, " == 1"))), nrow(.SD), .SDcols = cause_no_followup]]
  table1_coh[, p_9 := round(100 * n_9 / n_1, 1)]
  
  table1_coh[, n_10 := n_7 - n_8 - n_9]
  table1_coh[, p_10 := round(100 * n_10 / n_1, 1)]
  
  listvar <- grep("^n_", names(table1_coh), value = TRUE)
  listvar_p <- grep("^p_", names(table1_coh), value = TRUE)
  
  # cols_to_check <- setdiff(names(table1_coh), "ds")
  # table1_coh[, (cols_to_check) := lapply(.SD, function(x) fifelse(all(is.na(x)), 0, x)), .SDcols = cols_to_check]
  
  if (thisdatasource %in% datasources_stricter_masking) {
    
    count_ranges <- c("n_7", "n_10")
    percentage_ranges <- c("p_7", "p_10")
    listvar <- setdiff(listvar, c("n_1", count_ranges))
    
    # table1_coh[, n_1_check := 0]
    
    table1_coh <- mask_variables_per_threshold(table1_coh, listvar, threshold[[thisdatasource]], setdiff(listvar_p, percentage_ranges))
    
    table1_coh <- mask_values_against_recalculation(table1_coh, listvar, setdiff(listvar_p, percentage_ranges))

    table1_coh <- range_values_against_recalculation(table1_coh, count_ranges, percentage_ranges)
    
  } else {
    table1_coh <- mask_variables_per_threshold(table1_coh, listvar, threshold[[thisdatasource]])
  }
  
  table1_coh[, threshold := threshold[[thisdatasource]]]
  
  
  fwrite(table1_coh, paste0(thisdiroutput, "D5_Table_1_Attrition_", immdis, ".csv"))
  
}
