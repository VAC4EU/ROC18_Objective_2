
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
  thisdiroutput <- dirtemp
  immune_diseases_in_this_step <- immune_diseases_in_the_study
  # immune_diseases_in_this_step <- "E_GRAVES_AESI"
}

# load input datasets to be used by all diseases
vaccines <- get(load(paste0(thisdirinput, "D3_all_vaccines_curated.RData"))[[1]])
D4_source_population <- readRDS(paste0(thisdirinput2, "D4_source_population.rds"))

# Clean study population
D4_source_population <- D4_source_population[, .(person_id, death_date)]

# Clean vaccination by creating periods of 90 days after each vaccination and then merging them in spells
vaccines <- vaccines[, .(person_id, date_of_vax = date_curated)]
vaccines[, vax_end_window := date_of_vax %m+% lubridate::days(90 - day0)]
vaccines <- CreateSpells(vaccines, "person_id", "date_of_vax", "vax_end_window")
vaccines <- vaccines[, .(person_id, entry_spell_category, exit_spell_category)]
setnames(vaccines, c("entry_spell_category", "exit_spell_category"), c("date_of_vax", "vax_end_window"))

for (immdis in immune_diseases_in_this_step) {
  
  print(paste("Now calculating:", immdis))
  
  # Load cohort and flares
  cohort <- readRDS(paste0(thisdirinput, paste0("D3_cohort_", immdis, ".rds")))
  flares <- readRDS(paste0(thisdirinput, paste0("D3_flares_", immdis, ".rds")))
  
  # Keep only persons entering the followup and select columns
  simple_names <- c("entering_follow_up", "cohort_entry_date", "start_follow_up")
  setnames(cohort, paste(simple_names, immdis, sep = "_"), simple_names)
  cohort <- cohort[!is.na(entering_follow_up) & entering_follow_up == 1, ]
  cohort <- cohort[, .(person_id, cohort_entry_date, study_exit_date, start_follow_up)]
  
  # Attach to the cohort the date of death
  cohort <- D4_source_population[cohort, on = "person_id"]
  
  # Create a copy of start_follow_up and study_exit_date to be used at the end
  cohort[, copy_of_start_follow_up := start_follow_up][, copy_of_study_exit_date := study_exit_date]
  
  # Create TD dataset of vax in population
  pop_vax <- GenerateTDDataset(datasets = list(cohort[, in_population := 1],
                                               vaccines[, vax := 1]),
                               UoO_vars = c("person_id", "person_id"),
                               start_d_vars = c("copy_of_start_follow_up", "date_of_vax"),
                               end_d_vars = c("copy_of_study_exit_date", "vax_end_window"),
                               TD_variables = list(list("in_population"), list("vax")),
                               keep_auxiliary_variables = F,
                               keep_periods_observed_by = "first",
                               baseline_value = list("vax" = 0),
                               default_value_for_unobserved = list("vax" = 0)
  )
  
  # Clean resulting dataset
  pop_vax[, in_population := NULL]
  setnames(pop_vax, c("copy_of_start_follow_up", "copy_of_study_exit_date"), c("start_period", "end_period"))
  
  
  
  # Clean flares by creating periods of 90 days after each flare and then merging them in spells
  setnames(flares, paste("date_flare", immdis, sep = "_"), "date_of_flare")
  flares[, date_of_flare := as.Date(date_of_flare)]
  
  flares <- flares[pop_vax[vax == 0, .(person_id, start_period, end_period)],
                   .(person_id, date_of_flare = x.date_of_flare, start_period, end_period),
                   on = .(person_id, date_of_flare >= start_period, date_of_flare <= end_period), nomatch = NULL]
  flares <- flares[, .(person_id, date_of_flare = as.Date(date_of_flare))]
  flares[, dist := shift(date_of_flare), by = "person_id"]
  flares[is.na(dist), dist := date_of_flare]
  flares[, dist := as.integer(difftime(date_of_flare, dist, units = "days"))]
  
  flares[, sum_flares := Reduce(function(u, v) fifelse(u + v < 90, u + v, 0), dist, accumulate = TRUE),
                 by = "person_id"]
  if (!("sum_flares" %in% colnames(flares))) {
    flares[, sum_flares := integer()]
  }
  flares <- flares[sum_flares == 0, ][, .(person_id, date_of_flare)]
  
  flares[, date_of_flare := date_of_flare %m+% lubridate::days(1)]
  flares[, flare_end_window := date_of_flare %m+% lubridate::days(event_duration[[immdis]] - day0)]
  flares <- CreateSpells(flares, "person_id", "date_of_flare", "flare_end_window")
  flares <- flares[, .(person_id, entry_spell_category, exit_spell_category)]
  setnames(flares, c("entry_spell_category", "exit_spell_category"), c("date_of_flare", "flare_end_window"))
  
  # Create TD dataset of flares in population counting vax
  pop_vax_flares <- GenerateTDDataset(datasets = list(pop_vax,
                                                      flares[, flare := 1]),
                                      UoO_vars = c("person_id", "person_id"),
                                      start_d_vars = c("start_period", "date_of_flare"),
                                      end_d_vars = c("end_period", "flare_end_window"),
                                      TD_variables = list(list("vax"), list("flare")),
                                      keep_auxiliary_variables = F,
                                      keep_periods_observed_by = "first",
                                      baseline_value = list("flare" = 0),
                                      default_value_for_unobserved = list("flare" = 0)
  )
  
  # Keep track when persons are in followup
  pop_vax_flares[vax == 0 & flare == 0, in_followup := 1]
  
  # Shift vax e flares to get the reason why the previous periods ended
  loop_cols <- c("vax", "flare")
  pop_vax_flares[, (loop_cols) := lapply(loop_cols, function(x) shift(get(x), fill = 0, type = "lead"))]
  
  # Retain only periods of followup
  pop_vax_flares <- pop_vax_flares[in_followup == 1, ][, in_followup := NULL]
  
  # Find why the period ended
  pop_vax_flares <- pop_vax_flares[, cause_end_period := data.table::fcase(flare == 1, 5,
                                                                           vax == 1, 4,
                                                                           end_period == death_date, 2,
                                                                           end_period == study_end, 1,
                                                                           default = 3)]
  pop_vax_flares[, c("vax", "flare", "death_date") := NULL]
  
  # Create number of periods counter
  pop_vax_flares[, number_of_period := seq_len(.N), by = "person_id"]
  
  # Create counter for number of periods at risk for flare N
  pop_vax_flares[, number_of_period_at_risk_flare := cumsum(shift(fifelse(cause_end_period == 5, 1, 0), fill = 1)), by = "person_id"]
  
  # Reorder cols
  setcolorder(pop_vax_flares, c("person_id", "cohort_entry_date", "study_exit_date", "start_follow_up",
                                "number_of_period", "number_of_period_at_risk_flare"))
  
  # Use the correct name
  simple_names <- c("cohort_entry_date", "start_follow_up", "number_of_period", "number_of_period_at_risk_flare",
                    "cause_end_period")
  setnames(pop_vax_flares, simple_names, paste(simple_names, immdis, sep = "_"))
  
  # Use the correct name
  simple_names <- c("start_period", "end_period")
  setnames(pop_vax_flares, simple_names, paste(simple_names, immdis, "d", sep = "_"))
  
  # Save
  nameoutput <- paste0("D3_followup_periods_in_cohort_", immdis, ".rds")
  saveRDS(pop_vax_flares, file = file.path(thisdiroutput, nameoutput))
}
