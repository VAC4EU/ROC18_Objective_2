
if (TEST){ 
  testname <- "test_03_T2_50_create_periods_followup"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdirinput2 <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- paste0(file.path(dirtest, testname, "g_output_program"), "/")
  dir.create(thisdiroutput, showWarnings = F)
  immune_diseases_in_this_step <- c("E_GRAVES_AESI")
}else{
  thisdirinput <- diroutput
  thisdirinput2 <- dirtemp
  thisdiroutput <- diroutput
  immune_diseases_in_this_step <- immune_diseases_in_the_study
  # immdis <- "E_GRAVES_AESI"
}

for (immdis in immune_diseases_in_this_step) {
  
  # load input datasets to be used by all diseases
  persontime <- readRDS(paste0(thisdirinput, paste0("D4_persontime_", immdis, ".rds")))
  
  if (nrow(persontime) == 0) {
    empty_df <- data.table(number_of_period_at_risk_flare = NA_integer_, level_of_gender = NA_integer_,
                           level_of_ageband = NA_integer_, level_of_pregnancy = NA_integer_,
                           sex_at_instance_creation = NA_character_, ageband = NA_character_, pregnancy = NA_character_,
                           personyears = NA_integer_, flare = NA_integer_, N_persons = NA_integer_)
    # Use the correct name
    simple_names <- c("number_of_period_at_risk_flare", "personyears", "flare")
    setnames(empty_df, simple_names, paste(simple_names, immdis, sep = "_"))

    # Save
    nameoutput <- paste0("D4_persontime_Cube_", immdis, ".rds")
    saveRDS(empty_df[0, ], file = file.path(thisdiroutput, nameoutput))

    next
  }
  
  # assign the levels of each dimension
  assigned_levels <- vector(mode="list")
  assigned_levels[["Age"]] <- c("Ageband")
  assigned_levels[["CalendarTime"]] <- c("year")
  assigned_levels[["Gender"]] <- c("sex_at_instance_creation")
  assigned_levels[["number_of_period_at_risk_flare"]] <- c("number_of_period_at_risk_flare")
  assigned_levels[["pregnancy"]] <- c("pregnancy")
  
  # apply the function: note that the dimension Gender has its total computed, and that the statistics are not assigned, thus making Cube compute the default statistics (sum)
  
  # Use the correct name
  simple_names <- c("number_of_period_at_risk_flare", "personyears", "flare")
  setnames(persontime, paste(simple_names, immdis, sep = "_"), simple_names)
  
  persontime_Cube <- Cube(input = persontime,
                          dimensions = c("Age", "CalendarTime", "Gender"),
                          levels = assigned_levels,
                          measures = c("personyears", "flare"),
                          computetotal = c("Gender", "Age", "CalendarTime", "pregnancy")
  )
  
  persontime_Cube <- persontime_Cube[CalendarTime_LevelOrder == 99, ]
  persontime_Cube[, c("CalendarTime_LevelOrder", "CalendarTime_LabelValue", "number_of_period_at_risk_flare_LevelOrder") := NULL]
  
  setnames(persontime_Cube, c("personyears_sum", "flare_sum", "Age_LabelValue", "Gender_LabelValue", "pregnancy_LabelValue",
                              "number_of_period_at_risk_flare_LabelValue", "Age_LevelOrder", "Gender_LevelOrder", "pregnancy_LevelOrder"),
           c("personyears", "flare", "ageband", "sex_at_instance_creation", "pregnancy",
             "number_of_period_at_risk_flare", "level_of_ageband", "level_of_gender", "level_of_pregnancy"))
  persontime_Cube <- persontime_Cube[level_of_ageband == 99 | level_of_gender == 99, ]
  
  setcolorder(persontime_Cube, c("number_of_period_at_risk_flare", "level_of_gender", "level_of_ageband", "level_of_pregnancy",
                                 "sex_at_instance_creation", "ageband", "pregnancy", "personyears", "flare"))
  
  assigned_levels[["person_id"]] <- c("person_id")
  
  persontime_person_count <- Cube(input = persontime,
                                  dimensions = c("Age", "CalendarTime", "Gender"),
                                  levels = assigned_levels,
                                  measures = c("personyears", "flare"),
                                  computetotal = c("Gender", "Age", "CalendarTime", "pregnancy")
  )
  
  persontime_person_count <- persontime_person_count[CalendarTime_LevelOrder == 99, ]
  persontime_person_count[, c("CalendarTime_LevelOrder", "CalendarTime_LabelValue", "number_of_period_at_risk_flare_LevelOrder",
                              "person_id_LevelOrder") := NULL]
  
  setnames(persontime_person_count, c("personyears_sum", "flare_sum", "Age_LabelValue", "Gender_LabelValue", "pregnancy_LabelValue",
                                      "person_id_LabelValue", "number_of_period_at_risk_flare_LabelValue", "Age_LevelOrder",
                                      "Gender_LevelOrder", "pregnancy_LevelOrder"),
           c("personyears", "flare", "ageband", "sex_at_instance_creation", "pregnancy", "person_id",
             "number_of_period_at_risk_flare", "level_of_ageband", "level_of_gender", "level_of_pregnancy"))
  persontime_person_count <- persontime_person_count[level_of_ageband == 99 | level_of_gender == 99, ]
  # TODO change to != "0"
  persontime_person_count <- persontime_person_count[pregnancy != "0", ]
  
  setcolorder(persontime_person_count, c("number_of_period_at_risk_flare", "level_of_gender", "level_of_ageband", "level_of_pregnancy",
                                         "sex_at_instance_creation", "ageband", "pregnancy", "personyears", "flare"))
  
  persontime_person_count <- persontime_person_count[, .(personyears = sum(personyears), flare = sum(flare), N_persons = .N),
                                                     by = c("number_of_period_at_risk_flare", "level_of_gender", "level_of_ageband",
                                                            "level_of_pregnancy", "sex_at_instance_creation", "ageband", "pregnancy")]
  
  total_persontime <- persontime_Cube[persontime_person_count, on = colnames(persontime_Cube)]

  # Use the correct name
  simple_names <- c("number_of_period_at_risk_flare", "personyears", "flare")
  setnames(total_persontime, simple_names, paste(simple_names, immdis, sep = "_"))
  
  # Save
  nameoutput <- paste0("D4_persontime_Cube_", immdis, ".rds")
  saveRDS(total_persontime, file = file.path(thisdiroutput, nameoutput))
}
