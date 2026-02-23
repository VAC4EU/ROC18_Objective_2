if (TEST){
  testname <- "test_03_T2_50_create_periods_followup"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdirinput2 <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- paste0(file.path(dirtest, testname, "g_output_program"), "/")
  dir.create(thisdiroutput, showWarnings = F)
  immune_diseases_in_this_step <- c("E_GRAVES_AESI")
}else{
  thisdirinput <- dirtemp
  thisdirinput2 <- diroutput
  thisdirinput3 <- diroutput
  thisdiroutput <- diroutput
  immune_diseases_in_this_step <- immune_diseases_in_the_study
  # immdis <- "E_GRAVES_AESI"
}

# load input datasets to be used by all diseases
D4_source_population <- readRDS(paste0(thisdirinput3, "D4_source_population.rds"))

# Clean study population
D4_source_population <- D4_source_population[, .(person_id, birth_date, sex_at_instance_creation)]

for (immdis in immune_diseases_in_this_step) {
  
  # load input datasets to be used by all diseases
  cohort <- readRDS(paste0(thisdirinput, "D3_cohort_", immdis, ".rds"))
  
  # Keep only persons entering the followup and select columns
  simple_names <- c("entering_follow_up", "cohort_entry_date", "start_follow_up")
  setnames(cohort, paste(simple_names, immdis, sep = "_"), simple_names)
  
  cohort <- cohort[, .(person_id, cohort_entry_date, start_follow_up, entering_follow_up, study_exit_date)]
  
  # Attach to the cohort the date of birth
  cohort <- D4_source_population[cohort, on = "person_id"]
  
  if (nrow(cohort[!is.na(start_follow_up), ]) != 0) {
    # Create Ageband and persontime for cohort
    cohort[, ageband := age_fast(birth_date, cohort_entry_date)]
    cohort[, ageband := cut(ageband, c(0, 17, 59, 150), names(Agebands_countpersontime), right = TRUE)]
  } else {
    cohort[, ageband := NA_character_]
  }
  cohort[, birth_date := NULL]
  
  cohort_pre_followup_info <- copy(cohort)
  cohort_pre_followup_info[, exit_pre_followup := pmin(start_follow_up %m-% lubridate::days(1), study_exit_date, na.rm = T)]
  cohort_pre_followup_info <- cohort_pre_followup_info[, .(person_id, sex_at_instance_creation, ageband,
                                                           cohort_entry_date, exit_pre_followup)]
  
  # load input datasets to be used by all diseases
  flares <- readRDS(paste0(thisdirinput, paste0("D3_flares_", immdis, ".rds")))
  
  # Clean flares by creating periods of 90 days after each flare and then merging them in spells
  setnames(flares, paste("date_flare", immdis, sep = "_"), "date_of_flare")
  
  flares_pre_followup <- flares[cohort_pre_followup_info,
                                .(sex_at_instance_creation, ageband, cohort_entry_date,
                                  exit_pre_followup, x.date_of_flare),
                                on = .(person_id, date_of_flare > cohort_entry_date,
                                       date_of_flare <= exit_pre_followup), nomatch = NULL]
  
  # Create a placeholder to sum with Cube
  flares_pre_followup[, placeholder_2 := 1]
  
  # assign the levels of each dimension
  assigned_levels <- vector(mode="list")
  assigned_levels[["Age"]] <- c("ageband")
  assigned_levels[["Gender"]] <- c("sex_at_instance_creation")
  
  # apply the function: note that the dimension Gender has its total computed, and that the statistics are not assigned, thus making Cube compute the default statistics (sum)
  flares_pre_followup_cube <- Cube(input = flares_pre_followup,
                      dimensions = c("Age", "Gender"),
                      levels = assigned_levels,
                      measures = c("placeholder_2"),
                      computetotal = c("Gender", "Age")
  )
  
  setnames(flares_pre_followup_cube, c("placeholder_2_sum", "Age_LabelValue", "Gender_LabelValue", "Age_LevelOrder", "Gender_LevelOrder"),
           c("placeholder_2", "ageband", "sex_at_instance_creation", "level_of_ageband", "level_of_gender"))
  flares_pre_followup_cube <- flares_pre_followup_cube[level_of_ageband == 99 | level_of_gender == 99, ]
  
  cohort <- cohort[!is.na(entering_follow_up) & entering_follow_up == 0, ][, entering_follow_up := NULL]
  cohort[, study_exit_date := NULL]
  
  # assign the levels of each dimension
  assigned_levels <- vector(mode="list")
  assigned_levels[["Age"]] <- c("ageband")
  assigned_levels[["Gender"]] <- c("sex_at_instance_creation")
  
  # Create a placeholder to sum with Cube
  cohort[, placeholder := 1]
  
  # apply the function: note that the dimension Gender has its total computed, and that the statistics are not assigned, thus making Cube compute the default statistics (sum)
  cohort_cube <- Cube(input = cohort,
                      dimensions = c("Age", "Gender"),
                      levels = assigned_levels,
                      measures = c("placeholder"),
                      computetotal = c("Gender", "Age")
  )
  
  setnames(cohort_cube, c("placeholder_sum", "Age_LabelValue", "Gender_LabelValue", "Age_LevelOrder", "Gender_LevelOrder"),
           c("placeholder", "ageband", "sex_at_instance_creation", "level_of_ageband", "level_of_gender"))
  cohort_cube <- cohort_cube[level_of_ageband == 99 | level_of_gender == 99, ]
  
  persontime_cube <- readRDS(paste0(thisdirinput2, "D4_persontime_Cube_", immdis, ".rds"))
  persontime_cube <- persontime_cube[level_of_ageband == 99 | level_of_gender == 99, ]
  
  # Use the correct name
  simple_names <- c("number_of_period_at_risk_flare", "personyears", "flare")
  setnames(persontime_cube, paste(simple_names, immdis, sep = "_"), simple_names)
  
  persontime_cube[, c("personyears", "flare") := NULL]
  
  persontime_cube <- persontime_cube[level_of_pregnancy == 99 & number_of_period_at_risk_flare == 1, ]
  persontime_cube[, c("level_of_pregnancy", "pregnancy") := NULL]
  setnames(persontime_cube, "N_persons", "placeholder")
  
  total_cube <- rbindlist(list(persontime_cube, cohort_cube, flares_pre_followup_cube), use.names = T, fill = T)
  
  total_cube <- total_cube[, .(total_persons = sum(placeholder, na.rm = T),
                               flares_pre_followup = sum(placeholder_2, na.rm = T)),
                                     by = c("level_of_gender", "level_of_ageband", "sex_at_instance_creation", "ageband")]
  
  # Save
  nameoutput <- paste0("D4_population_Cube_", immdis, ".rds")
  saveRDS(total_cube, file = file.path(thisdiroutput, nameoutput))
  
}