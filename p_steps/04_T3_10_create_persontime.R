
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
  thisdiroutput <- diroutput
  immune_diseases_in_this_step <- immune_diseases_in_the_study
  # immdis <- "E_GRAVES_AESI"
}

# load input datasets to be used by all diseases
# vaccines <- get(load(paste0(thisdirinput, "D3_all_vaccines_curated.RData"))[[1]])
D4_source_population <- readRDS(paste0(thisdirinput2, "D4_source_population.rds"))

# Clean study population
D4_source_population <- D4_source_population[, .(person_id, birth_date, death_date, sex_at_instance_creation)]

# # Clean vaccination by creating periods of 90 days after each vaccination and then merging them in spells
# # date_of_flare since it's easier to use during a future join
# vaccines <- vaccines[, .(person_id, date_of_flare = date_curated)]

for (immdis in immune_diseases_in_this_step) {
  
  print(immdis)
  
  # Load cohort and flares
  periods <- readRDS(paste0(thisdirinput, paste0("D3_followup_periods_in_cohort_with_pregnancy_", immdis, ".rds")))
  flares <- readRDS(paste0(thisdirinput, paste0("D3_flares_", immdis, ".rds")))
  
  # Keep only persons entering the followup and select columns
  simple_names <- c("number_of_period_at_risk_flare", "pregnancy")
  setnames(periods, paste(simple_names, immdis, sep = "_"), simple_names)
  
  simple_names <- c("start_period", "end_period")
  setnames(periods, paste(simple_names, immdis, "d", sep = "_"), simple_names)
  
  periods <- periods[, .(person_id, start_period, end_period, number_of_period_at_risk_flare, pregnancy)]
  
  # # Join flres and vaccination to get all vaccination on the same day of a flare
  setnames(flares, paste("date_flare", immdis, sep = "_"), "date_of_flare")
  # flares[, date_of_flare := as.Date(date_of_flare)]
  # flares <- vaccines[, flag := 1][flares, on = c("person_id", "date_of_flare")]
  # 
  # # Remove flares on the same day of a vaccination
  # flares <- flares[is.na(flag), ][, flag := NULL]
  # 
  # # Clean flares using non-equi join with periods
  # flares <- periods[flares, .(person_id, date_of_flare, x.start_period, x.end_period), 
  #         on = .(person_id, start_period <= date_of_flare, end_period >= date_of_flare)]
  # flares <- flares[!is.na(x.start_period), ][, c("x.start_period", "x.end_period") := NULL]
  
  # # Assign number to each flare and change name of event and retain until 3
  # flares[, name_of_event := seq_len(.N), by = "person_id"]
  # flares <- flares[name_of_event <= 3, ]
  # flares[, name_of_event := paste0("Flare_", name_of_event)]
  
  # Attach to the cohort the date of birth and death
  periods <- D4_source_population[periods, on = "person_id"]
  
  if (nrow(periods) > 0) {
    max_exit <- periods[, ceiling_date(max(end_period), 'year') %m-% days(1)]
  } else {
    empty_df <- data.table(number_of_period_at_risk_flare = NA_integer_, sex_at_instance_creation = NA_character_,
                           pregnancy = NA_integer_, year = NA_integer_, Ageband = NA_character_, 
                           personyears = NA_integer_, flare = NA_integer_)
    
    # Use the correct name
    simple_names <- c("number_of_period_at_risk_flare", "personyears", "flare")
    setnames(empty_df, simple_names, paste(simple_names, immdis, sep = "_"))
    
    # Save
    nameoutput <- paste0("D4_persontime_", immdis, ".rds")
    saveRDS(empty_df[0, ], file = file.path(thisdiroutput, nameoutput))
    next
  }
  
  # CPT_outcomes <- paste0("Flare_", 1:3)
  vars_to_add <- data.table(person_id = periods[1, person_id], date_of_flare = ymd(99991231), name_of_event = "Flare")
  flares <- rbindlist(list(flares[, name_of_event := "Flare"][, date_of_flare := as.Date(date_of_flare)], vars_to_add))
  
  # CHECK FLARES AND VACCINATION PERIODS
  persontime_flares <- CountPersonTime(
    Dataset_events = flares,
    Dataset = periods,
    Person_id = "person_id",
    Start_study_time = gsub('-', '', as.character(study_start)),
    End_study_time = gsub('-', '', as.character(max_exit)),
    Start_date = "start_period",
    End_date = "end_period",
    Birth_date = "birth_date",
    Strata = c("sex_at_instance_creation", "number_of_period_at_risk_flare", "pregnancy"),
    Name_event = "name_of_event",
    Date_event = "date_of_flare",
    Age_bands = Agebands_countpersontime,
    Increment = "year",
    Outcomes_rec = "Flare",
    Rec_period = 1,
    Unit_of_age = "year",
    include_remaning_ages = T,
    Aggregate = F
  )
  
  persontime_flares[, Persontime := NULL]
  persontime_flares <- persontime_flares[number_of_period_at_risk_flare <= 3, ]
  
  setnames(persontime_flares, c("Persontime_Flare", "Flare_b"), c("personyears", "flare"))
  
  persontime_flares[, personyears := personyears / 365.25]
  # colA = paste0("Persontime_Flare_", 1:3)
  # colB = paste0("Flare_", 1:3, "_b")
  # 
  # persontime_flares[, Persontime := NULL]
  # persontime_flares_melted = melt(persontime_flares, measure = list(colA, colB), value.name = c("personyears", "flare"),
  #                                 variable.name = "number_of_period", variable.factor = F)

  setcolorder(persontime_flares, "number_of_period_at_risk_flare")
  
  # Use the correct name
  simple_names <- c("number_of_period_at_risk_flare", "personyears", "flare")
  setnames(persontime_flares, simple_names, paste(simple_names, immdis, sep = "_"))
  
  # Save
  nameoutput <- paste0("D4_persontime_", immdis, ".rds")
  saveRDS(persontime_flares, file = file.path(thisdiroutput, nameoutput))
}
