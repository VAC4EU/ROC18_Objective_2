
if (TEST){ 
  testname <- "05_T4_20_Table_2_Cohort_characteristics"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdirinput2 <- paste0(file.path(dirtest, testname), "/")
  thisdirinput3 <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- paste0(file.path(dirtest, testname, "g_output_program"), "/")
  dir.create(thisdiroutput, showWarnings = F)
  immune_diseases_in_this_step <- c("E_GRAVES_AESI")
}else{
  thisdirinput <- dirtemp
  thisdirinput2 <- diroutput
  thisdirinput3 <- dirTD
  thisdiroutput <- dirtablesubpop[[subpop]]
  immune_diseases_in_this_step <- immune_diseases_in_the_study
  # immune_diseases_in_this_step <- "E_GRAVES_AESI"
}

# Table 2

print("Now creating: Table 2")

# Load files
full_pop <- readRDS(paste0(thisdirinput2, "D4_source_population.rds"))

for (immdis in immune_diseases_in_this_step) {
  print(immdis)
  base_table2 <- data.table::data.table(ds = thisdatasource)
  cohort <- readRDS(file = file.path(thisdirinput, paste0("D3_cohort_", immdis, ".rds")))
  
  simple_names <- c("has_not_a_code_in_the_study_period", "exclude_because_exist_code_during_lookback",
                      "exclude_because_exist_exclusion_criterion_during_lookback", "enter_cohort", 
                      "cohort_entry_date", "cause_for_not_entering_followup", 
                      "entering_follow_up_postponed", "entering_follow_up", "start_follow_up")
  setnames(cohort, paste(simple_names, immdis, sep = "_"), simple_names)
  
  # Remove persons not entering the cohort
  cohort <- cohort[has_not_a_code_in_the_study_period == 0 & exclude_because_exist_code_during_lookback == 0 &
                     exclude_because_exist_exclusion_criterion_during_lookback == 0, ]
  
  # First attach relevant infromation from the total population to the cohort
  full_pop <- full_pop[, .(person_id, birth_date, sex_at_instance_creation)]
  cohort <- full_pop[cohort, on = "person_id"]
  
  # Total population
  base_table2[, n_1 := cohort[, .N]]
  
  # Male/Female/Other
  base_table2[, n_2 := cohort[sex_at_instance_creation == "F", .N]]
  base_table2[, p_2 := round(100 * n_2 / n_1, 1)]
  
  base_table2[, n_3 := cohort[sex_at_instance_creation == "M", .N]]
  base_table2[, p_3 := round(100 * n_3 / n_1, 1)]
  
  base_table2[, n_4 := cohort[sex_at_instance_creation %not in% c("F", "M"), .N]]
  base_table2[, p_4 := round(100 * n_4 / n_1, 1)]
  
  # Calculate age at entrance into cohort
  cohort[, age_at_cohort_entry := age_fast(birth_date, cohort_entry_date)]
  
  # Calculate quartile of age
  base_table2[, pc_5 := fifelse(thisdatasource %not in% datasources_stricter_masking,
                                cohort[, quantile(age_at_cohort_entry, .25)], NA_integer_)]
  base_table2[, pc_6 := fifelse(thisdatasource %not in% datasources_stricter_masking,
                                cohort[, quantile(age_at_cohort_entry, .50)], NA_integer_)]
  base_table2[, pc_7 := fifelse(thisdatasource %not in% datasources_stricter_masking,
                                cohort[, quantile(age_at_cohort_entry, .75)], NA_integer_)]
  
  # Calculate in agebands
  base_table2[, n_8 := cohort[age_at_cohort_entry < 18, .N]]
  base_table2[, p_8 := round(100 * n_8 / n_1, 1)]
  
  # DO NOT USE between. It fails if the dataset is empty
  base_table2[, n_9 := cohort[18 <= age_at_cohort_entry &  age_at_cohort_entry <= 59, .N]]
  base_table2[, p_9 := round(100 * n_9 / n_1, 1)]
  
  base_table2[, n_10 := cohort[age_at_cohort_entry > 59, .N]]
  base_table2[, p_10 := round(100 * n_10 / n_1, 1)]
  
  # Number of people in follow-up
  base_table2[, n_11 := cohort[!is.na(entering_follow_up) & entering_follow_up == 1, .N]]
  base_table2[, p_11 := round(100 * n_11 / n_1, 1)]
  
  # Total follow-up calculated for all intervals
  cohort[entering_follow_up == 1, tot_fup := correct_difftime(study_exit_date, start_follow_up)]
  base_table2[, py_12 := round(as.numeric(cohort[, sum(tot_fup, na.rm = T)] / 365.25), 0)]
  
  # Load vaccinations
  D3_all_vaccines_curated <- get(load(paste0(thisdirinput, "D3_all_vaccines_curated.RData"))[[1]])
  
  # Add vaccinations to cohort
  simple_cohort <- cohort[, .(person_id, study_exit_date, start_follow_up, entering_follow_up)]
  simple_cohort <- simple_cohort[!is.na(entering_follow_up) & entering_follow_up == 1,]
  
  simple_cohort_for_preg <- copy(simple_cohort)[, .(person_id, study_exit_date, start_follow_up)]
  
  simple_cohort <- D3_all_vaccines_curated[, .(person_id, date_curated)][simple_cohort, on = "person_id"]
  
  # Transform vaccination not in total followup to NA
  simple_cohort[!between(date_curated, start_follow_up, study_exit_date), date_curated := NA]
  
  # Create a flag to help keeping track of persons without any vaccination
  simple_cohort[, flag := fifelse(is.na(date_curated), 1, 0)]
  
  # Keep only rows with the minimum flag: if a persons has vaccination keep only records with one
  if (nrow(simple_cohort) != 0) {
    simple_cohort <- simple_cohort[simple_cohort[, .I[flag == min(flag)], by = "person_id"]$V1]
  }
  
  # Count the number of records
  simple_cohort <- simple_cohort[, .N, by = c("person_id", "flag")]
  
  # If a persons didn't any eligible vaccination then input manually the count to 0
  simple_cohort[flag == 1, N := 0][, flag := NULL]
  
  # Quartile of number of vaccinations
  base_table2[, pc_13 := fifelse(thisdatasource %not in% datasources_stricter_masking,
                                 simple_cohort[, quantile(N, .50)], NA_integer_)]
  base_table2[, pc_13_2 := fifelse(thisdatasource %not in% datasources_stricter_masking,
                                   simple_cohort[, quantile(N, .25)], NA_integer_)]
  base_table2[, pc_13_3 := fifelse(thisdatasource %not in% datasources_stricter_masking,
                                   simple_cohort[, quantile(N, .75)], NA_integer_)]
  
  base_table2[, n_14 := simple_cohort[N == 0, .N]]
  base_table2[, p_14 := round(100 * n_14 / n_1, 1)]
  base_table2[, n_15 := simple_cohort[N == 1, .N]]
  base_table2[, p_15 := round(100 * n_15 / n_1, 1)]
  base_table2[, n_16 := simple_cohort[N == 2, .N]]
  base_table2[, p_16 := round(100 * n_16 / n_1, 1)]
  base_table2[, n_17 := simple_cohort[N >= 3, .N]]
  base_table2[, p_17 := round(100 * n_17 / n_1, 1)]
  
  pregnancy_df <- readRDS(file = paste0(thisdirinput3, paste0("D3_TD_PREGNANCY.rds")))
  preg_in_cohort <- pregnancy_df[simple_cohort_for_preg, .(person_id, x.date, start_follow_up, study_exit_date),
               on = .(person_id, date >= start_follow_up, date <= study_exit_date)]
  
  base_table2[, n_18 := length(unique(preg_in_cohort[!is.na(x.date), person_id]))]
  base_table2[, p_18 := round(100 * n_18 / n_1, 1)]
  
  listvar <- grep("^n_", names(base_table2), value = TRUE)
  listvar_p <- grep("^p_", names(base_table2), value = TRUE)
  
  percentage_vars <- setdiff(listvar, c("n_1", "n_11"))
  names(percentage_vars) <- setdiff(listvar_p, "p_11")
  
  if (thisdatasource %in% datasources_stricter_masking) {
    
    if (thisdatasource %in% c("TEST", "DANREG")) {
      base_table2 <- range_values_against_recalculation(base_table2, c("n_2", "n_3", "n_4"), c("p_2", "p_3", "p_4"))
      base_table2 <- range_values_against_recalculation(base_table2, c("n_8", "n_9", "n_10"), c("p_8", "p_9", "p_10"))
      base_table2 <- range_values_against_recalculation(base_table2, c("n_14", "n_15", "n_16", "n_17"), c("p_14", "p_15", "p_16", "p_17"))
      
      base_table2 <- mask_variables_per_threshold(base_table2, "n_18", threshold[[thisdatasource]], percentage_vars["p_18"])
    } else {
      base_table2 <- mask_variables_per_threshold(base_table2, setdiff(listvar, c("n_1", "n_11")), threshold[[thisdatasource]], percentage_vars)
      
      base_table2 <- mask_values_against_recalculation(base_table2, c("n_2", "n_3", "n_4"), c("p_2", "p_3", "p_4"))
      base_table2 <- mask_values_against_recalculation(base_table2, c("n_8", "n_9", "n_10"), c("p_8", "p_9", "p_10"))
      base_table2 <- mask_values_against_recalculation(base_table2, c("n_14", "n_15", "n_16", "n_17"), c("p_14", "p_15", "p_16", "p_17"))
    }
    
    base_table2 <- range_values_against_recalculation(base_table2, "n_1")
    base_table2 <- range_values_against_recalculation(base_table2, "n_11", "p_11")
    base_table2 <- range_values_against_recalculation(base_table2, "py_12")
    
  } else {
    base_table2 <- mask_variables_per_threshold(base_table2, listvar, threshold[[thisdatasource]])
  }
  
  base_table2[, threshold := threshold[[thisdatasource]]]
  
  fwrite(base_table2, paste0(thisdiroutput, "D5_Table_2_Cohort_characteristics_", immdis, ".csv"))
}
