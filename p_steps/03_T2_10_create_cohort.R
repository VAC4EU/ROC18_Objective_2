############################################################
#                                                          #
####   CREATE D3_cohort_{ImmDis} ####
# 
#                                                          #
############################################################

# author: Davide Messina
# 
# v 0.0.1  XXX

#########################################

#########################################
# assign input and output directories

# TODO check case when vax and event same date

if (TEST){
  total_immune_diseases <- c("E_GRAVES_AESI", "Im_HASHIMOTO_AESI", "SK_ERYTHEMANODOSUM_AESI", "Im_HASHIMOTO_AESI_sensitivity")
  max_iter <- length(total_immune_diseases)
} else {
  max_iter <- 1
}

for (test_i in 1:max_iter) {
  if (TEST){
    immune_diseases_in_this_step <- total_immune_diseases[[test_i]]
    suffix_test <- data.table::fcase(immune_diseases_in_this_step == "E_GRAVES_AESI", "GRAVES",
                                     immune_diseases_in_this_step == "Im_HASHIMOTO_AESI", "HM",
                                     immune_diseases_in_this_step == "SK_ERYTHEMANODOSUM_AESI", "EN",
                                     immune_diseases_in_this_step == "Im_HASHIMOTO_AESI_sensitivity", "HM_sensitivity")
    testname <- paste("test_03_T2_10_create_cohort", suffix_test, sep = "_")
    thisdirinput <- paste0(file.path(dirtest, testname), "/")
    thisdirinput2 <- paste0(file.path(dirtest, testname), "/")
    thisdirinput3 <- paste0(file.path(dirtest, testname), "/")
    thisdiroutput <- paste0(file.path(dirtest, testname, "g_output_program"), "/")
    dir.create(thisdiroutput, showWarnings = F)
  } else {
    thisdirinput <- dirtemp
    thisdirinput2 <- diroutput
    thisdirinput3 <- dirconceptsets
    thisdiroutput <- dirtemp
    immune_diseases_in_this_step <- immune_diseases_in_the_study
  }
  
  # load input datasets to be used by all diseases
  D3_all_vaccines_curated <- get(load(paste0(thisdirinput, "D3_all_vaccines_curated.RData"))[[1]])
  D4_source_population <- readRDS(paste0(thisdirinput2, "D4_source_population.rds"))
  
  # Clean study population
  D4_source_population[, c("birth_date", "sex_at_instance_creation", "spell_start_date"):= NULL]
  
  # Clean vaccination by creating periods of 90 days after each vaccination and then merging them in spells
  D3_all_vaccines_curated <- D3_all_vaccines_curated[, .(person_id, date_of_vax = date_curated)]
  D3_all_vaccines_curated[, vax_end_window := date_of_vax %m+% (lubridate::days(90 - day0))]
  D3_all_vaccines_curated <- CreateSpells(D3_all_vaccines_curated, "person_id", "date_of_vax", "vax_end_window")
  D3_all_vaccines_curated <- D3_all_vaccines_curated[, .(person_id, entry_spell_category, exit_spell_category)]
  setnames(D3_all_vaccines_curated, c("entry_spell_category", "exit_spell_category"), c("date_of_vax", "vax_end_window"))
  
  for (immdis in immune_diseases_in_this_step){
    
    #########################################
    # load disease-specific input dataset
    correct_concept_name <- data.table::fcase(immdis == "Im_HASHIMOTO_AESI_sensitivity", "Im_HASHIMOTO_AESI",
                                              immdis == "N_DEMYELMS_AESI_sensitivity", "N_DEMYELMS_AESI",
                                              default = immdis)
    events <- get(load(paste0(thisdirinput3, paste0(correct_concept_name, "_narrow.RData")))[[1]])
    events <- events[, .(person_id, date_of_event = date, meaning_renamed)]
    
    #########################################
    # process input datasets to obtain output
    for (prm in names(select_in_group_of_prompts)) {
      
      # To be used here and after to rename the columns
      old_cols_names <- c("has_not_a_code_in_the_study_period", "exclude_because_exist_code_during_lookback",
                          "exclude_because_exist_exclusion_criterion_during_lookback", "enter_cohort", 
                          "cohort_entry_date", "cause_for_not_entering_followup", 
                          "entering_follow_up_postponed", "entering_follow_up", "start_follow_up")
      new_cols_names <- paste0(old_cols_names, "_", immdis)
      if (prm != "all") new_cols_names <- paste0(new_cols_names, "_", prm)
      
      if (prm %not in% c("all", groups_of_prompts_thisdatasource)) {
        pop_with_events_vacc[, (new_cols_names) := NA]
        next
      }
      
      # Restrict events to only ones in this prompt
      if (thisdatasource == "TEST" & TEST == F) {
        restricted_events <- copy(events)
      } else {
        restricted_events <- copy(events)[eval(parse(text = select_in_group_of_prompts[[prm]])), ]
      }
      
      # Attach persons information to only retain events in the study period and filter the first one
      restricted_events <- D4_source_population[restricted_events, on = "person_id"]
      
      # Remove events of persons not in population
      restricted_events <- restricted_events[!is.na(study_entry_date) & !is.na(date_of_event), ]
      
      way_back_date <- ymd(19000101)
      restricted_events[, start_lookback_long := study_entry_date %m-% lubridate::days(lookback_period)]
      restricted_events <- restricted_events[data.table::between(date_of_event, start_lookback_long, study_exit_date), ]
      restricted_events[, exist_event_in_study := data.table::between(date_of_event, study_entry_date, study_exit_date)]
      restricted_events <- restricted_events[, .(date_of_event = min(date_of_event), exist_event_in_study = max(exist_event_in_study)), by = c("person_id", "start_lookback_long")]
      restricted_events <- restricted_events[exist_event_in_study == 1, ]
      
      if (prm == "all") {
        # Keep only persons with at least an event
        pop_with_events <- D4_source_population[restricted_events, on = "person_id"]
      } else {
        # Or keep all persons and attach events
        pop_with_events <- restricted_events[pop_with_events_vacc, on = "person_id"]
      }
      
      # Check if persons with event inside observable window (lookback + study period)
      pop_with_events[, has_not_a_code_in_the_study_period := fifelse(exist_event_in_study == 1,
                                                                      0, 1, na = 1)]
      pop_with_events[, exist_event_in_study := NULL]
      
      # Check if persons with event inside lookback
      pop_with_events[!is.na(start_lookback_long), exclude_because_exist_code_during_lookback := fifelse(data.table::between(date_of_event,
                                                                                                                 start_lookback_long,
                                                                                                                 study_entry_date %m-% lubridate::days(1)),
                                                                                                         1, 0)]
      pop_with_events[, start_lookback_long := NULL]
      
      # Check if persons with special condition inside lookback
      if (immdis %in% c("Im_HASHIMOTO_AESI", "SK_ERYTHEMANODOSUM_AESI", "N_DEMYELMS_AESI",
                        "Im_HASHIMOTO_AESI_sensitivity", "N_DEMYELMS_AESI_sensitivity")) {
        
        lookback_period_additional <- 365
        
        # Load concepts to use for exclusion
        if (immdis %in% c("Im_HASHIMOTO_AESI", "Im_HASHIMOTO_AESI_sensitivity")) {
          events_excl <- get(load(paste0(thisdirinput3, "DP_LEVOTHYROXINE.RData"))[[1]])
        } else if (immdis == "SK_ERYTHEMANODOSUM_AESI") {
          concepts_to_load <- c("Sk_PANNICULITIS_AESI")
          events_excl <- lapply(concepts_to_load, function(x) {
            get(load(paste0(thisdirinput3, paste0(x, "_narrow.RData")))[[1]])
          })
          events_excl <- rbindlist(events_excl)
          # Restrict events to only ones in this prompt and only for diagnoses
          if (thisdatasource == "TEST" & TEST == F) {
            events_excl <- events_excl
          } else {
            events_excl <- events_excl[eval(parse(text = select_in_group_of_prompts[[prm]])), ]
          }
        } else if (immdis %in% c("N_DEMYELMS_AESI", "N_DEMYELMS_AESI_sensitivity")) {
          concepts_to_load <- c("DP_INTERFERONBETA1A1B", "DP_GLATIRAMER", "DP_TERIFLUNOMIDE")
          events_excl <- lapply(concepts_to_load, function(x) {
            get(load(paste0(thisdirinput3, paste0(x, ".RData")))[[1]])
          })
          events_excl <- rbindlist(events_excl)
        }
        
        if (immdis %in% c("Im_HASHIMOTO_AESI_sensitivity", "N_DEMYELMS_AESI_sensitivity")) {
          end_lookback_period <- 183
          lookback_period_additional <- 99999
        } else {
          end_lookback_period <- 1
        }
        
        # Retain only persons_id and date then keep all events and attach persons information
        events_excl <- events_excl[, .(person_id, date_of_excl_event = date)]
        filtered_events <- pop_with_events[events_excl, on = "person_id"]
        
        flag_empty_df <- fifelse(immdis %in% c("Im_HASHIMOTO_AESI", "SK_ERYTHEMANODOSUM_AESI", "N_DEMYELMS_AESI",
                                               "Im_HASHIMOTO_AESI_sensitivity") & nrow(filtered_events) == 0, T, F)
        
        # Remove events of persons not in population
        filtered_events <- filtered_events[!is.na(study_entry_date), ]
        
        # Create base exclusion criteria for a single diagnosis/medication
        filtered_events[, excl_criteria := fifelse(data.table::between(date_of_excl_event,
                                                           date_of_event %m-% lubridate::days(lookback_period_additional),
                                                           date_of_event %m-% lubridate::days(end_lookback_period), NAbounds = FALSE),
                                                   1, 0)]
        
        # Calculate if the person has at least one record positive for the exclusion criteria
        filtered_events <- filtered_events[, .(exclude_because_exist_exclusion_criterion_during_lookback = max(excl_criteria, na.rm = T)),
                                           by = "person_id"]
        
        # Attach the information to the population dataset and set the generated missing value to 0
        pop_with_events <- merge(pop_with_events, filtered_events, all.x = T, by = "person_id")
        pop_with_events[is.na(exclude_because_exist_exclusion_criterion_during_lookback),
                        exclude_because_exist_exclusion_criterion_during_lookback := ifelse(flag_empty_df, 1, 0)]
        
      } else {
        pop_with_events[has_not_a_code_in_the_study_period == 0, exclude_because_exist_exclusion_criterion_during_lookback := 0]
      }
      
      # Calculate if person enters the cohort and if yes when
      pop_with_events[, enter_cohort := fifelse(has_not_a_code_in_the_study_period == 0 &
                                                  exclude_because_exist_code_during_lookback == 0 &
                                                  exclude_because_exist_exclusion_criterion_during_lookback == 0, 1, 0)]
      pop_with_events[enter_cohort == 1, cohort_entry_date := date_of_event]
      date1970 <- ymd(19700101)
      pop_with_events[is.na(date_of_event), date_of_event := date1970]
      
      # Preparation for foverlaps
      # Join vaccination dates which overlap the entrance of the study for each person (event + 90 days)
      pop_with_events[, event_next_day := date_of_event %m+% lubridate::days(1)]
      pop_with_events[, event_end_window := date_of_event %m+% lubridate::days(event_duration[[immdis]] - day0)]
      setkey(pop_with_events, person_id, event_next_day, event_end_window)
      setkey(D3_all_vaccines_curated, person_id, date_of_vax, vax_end_window)
      
      pop_with_events_vacc <- foverlaps(pop_with_events, D3_all_vaccines_curated, by.x = c("person_id", "event_next_day", "event_end_window"), by.y = c("person_id", "date_of_vax", "vax_end_window"))
      
      # Cleaning after foverlaps
      pop_with_events_vacc[, event_next_day := NULL]
      pop_with_events_vacc[date_of_event == date1970, date_of_event := NA]
      
      pop_with_events_vacc <- pop_with_events_vacc[, .(date_of_vax = max(date_of_vax), vax_end_window = max(vax_end_window)), by = setdiff(colnames(pop_with_events_vacc), c("date_of_vax", "vax_end_window"))]
      
      # Reasons for entering followup
      # TODO modify event_end_window to vax_end_window (check now solutions, also start of followup)
      pop_with_events_vacc[!is.na(date_of_event) & enter_cohort == 1,
                           cause_for_not_entering_followup := fcase(study_exit_date <= pmax(event_end_window, vax_end_window, na.rm = T) &
                                                                      study_exit_date == death_date, 2L,
                                                                    study_exit_date <= pmax(event_end_window, vax_end_window, na.rm = T), 1L,
                                                                    default = 0L)]
      
      # Is any enter of followup postponed
      pop_with_events_vacc[!is.na(date_of_event) & enter_cohort == 1,
                           entering_follow_up_postponed := fifelse(!is.na(date_of_vax) &
                                                                     cause_for_not_entering_followup == 0, 1, 0)]
      
      # General entrance of followup
      pop_with_events_vacc[, entering_follow_up := fifelse(cause_for_not_entering_followup == 0, 1, 0)]
      pop_with_events_vacc[entering_follow_up == 1, start_follow_up := pmax(event_end_window, vax_end_window, na.rm = T) %m+% lubridate::days(1)]
      
      pop_with_events_vacc[, c("date_of_vax", "event_end_window", "vax_end_window", "date_of_event") := NULL]
      
      setnames(pop_with_events_vacc, old_cols_names, new_cols_names)
      
    }
    
    pop_with_events_vacc[, death_date := NULL]
    
    # #########################################
    # # save
    
    nameoutput <- paste0("D3_cohort_", immdis, ".rds")
    saveRDS(pop_with_events_vacc, file = file.path(thisdiroutput, nameoutput))
  }
}
