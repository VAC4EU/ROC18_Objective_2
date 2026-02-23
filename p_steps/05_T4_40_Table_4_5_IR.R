if (TEST){
  testname <- "test_05_T4_40_Table_4_5"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdirinput2 <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- paste0(file.path(dirtest, testname, "g_output_program"), "/")
  dir.create(thisdiroutput, showWarnings = F)
  immune_diseases_in_this_step <- c("E_GRAVES_AESI")
}else{
  thisdirinput <- diroutput
  thisdirinput2 <- direxp
  thisdirinput3 <- diroutput
  thisdiroutput <- dirtablesubpop[[subpop]]
  immune_diseases_in_this_step <- immune_diseases_in_the_study
  # immdis <- "E_GRAVES_AESI"
}

strata <- list()
strata[["all"]] <- "level_of_gender == 99 & level_of_ageband == 99"
strata[["M-allage"]] <- "level_of_gender == 1 & level_of_ageband == 99 & sex_at_instance_creation == 'M'"
strata[["F-allage"]] <- "level_of_gender == 1 & level_of_ageband == 99 & sex_at_instance_creation == 'F'"
strata[["allgender-0"]] <- "level_of_gender == 99 & level_of_ageband == 1 & ageband == '0-17'"
strata[["allgender-18"]] <- "level_of_gender == 99 & level_of_ageband == 1 & ageband == '18-59'"
strata[["allgender-60"]] <- "level_of_gender == 99 & level_of_ageband == 1 & ageband == '60+'"

base_table_4_5 <- data.table::data.table(ds = thisdatasource)

for (immdis in immune_diseases_in_this_step) {
  
  # load input datasets to be used by all diseases
  pop_cube <- readRDS(paste0(thisdirinput, "D4_population_Cube_", immdis, ".rds"))
  
  pt_cube <- readRDS(paste0(thisdirinput, "D4_persontime_Cube_", immdis, ".rds"))
  pt_cube <- pt_cube[level_of_ageband == 99 | level_of_gender == 99, ]
  
  for (stratum in names(strata)) {
    
    pop_cube_restricted <- copy(pop_cube)[eval(parse(text = strata[[stratum]]))]
    
    pt_cube_stratum <- copy(pt_cube)[eval(parse(text = strata[[stratum]]))]
    
    # Use the correct name
    simple_names <- c("number_of_period_at_risk_flare", "personyears", "flare")
    setnames(pt_cube_stratum, paste(simple_names, immdis, sep = "_"), simple_names)
    
    name_cols <- c("IR", "lb", "ub")
    pt_cube_stratum[, (name_cols) := exactPoiCI(pt_cube_stratum, "flare", "personyears", conversion_factor = 1)]
    
    table_4_5 <- copy(base_table_4_5)
    
    table_4_5[, n_1 := pop_cube_restricted[, total_persons]]
    table_4_5[, n_2 := pt_cube_stratum[number_of_period_at_risk_flare == 1 & level_of_pregnancy == 99, N_persons]]
    table_4_5[, n_3 := pop_cube_restricted[, flares_pre_followup]]
    
    table_4_5[, m_1_0 := pt_cube_stratum[number_of_period_at_risk_flare == 1 & level_of_pregnancy == 99, flare]]
    table_4_5[, ir_1 := pt_cube_stratum[number_of_period_at_risk_flare == 1 & level_of_pregnancy == 99, IR]]
    table_4_5[, ll_1 := pt_cube_stratum[number_of_period_at_risk_flare == 1 & level_of_pregnancy == 99, lb]]
    table_4_5[, ul_1 := pt_cube_stratum[number_of_period_at_risk_flare == 1 & level_of_pregnancy == 99, ub]]
    
    table_4_5[, npr_1 := pt_cube_stratum[number_of_period_at_risk_flare == 1 & level_of_pregnancy == 1, N_persons]]
    table_4_5[, mpr_1 := pt_cube_stratum[number_of_period_at_risk_flare == 1 & level_of_pregnancy == 1, flare]]
    table_4_5[, irpr_1 := pt_cube_stratum[number_of_period_at_risk_flare == 1 & level_of_pregnancy == 1, IR]]
    table_4_5[, llpr_1 := pt_cube_stratum[number_of_period_at_risk_flare == 1 & level_of_pregnancy == 1, lb]]
    table_4_5[, ulpr_1 := pt_cube_stratum[number_of_period_at_risk_flare == 1 & level_of_pregnancy == 1, ub]]
    
    table_4_5[, n_1_2 := pt_cube_stratum[number_of_period_at_risk_flare == 2 & level_of_pregnancy == 99, N_persons]]
    table_4_5[, m_2_0 := pt_cube_stratum[number_of_period_at_risk_flare == 2 & level_of_pregnancy == 99, flare]]
    table_4_5[, ir_2 := pt_cube_stratum[number_of_period_at_risk_flare == 2 & level_of_pregnancy == 99, IR]]
    table_4_5[, ll_2 := pt_cube_stratum[number_of_period_at_risk_flare == 2 & level_of_pregnancy == 99, lb]]
    table_4_5[, ul_2 := pt_cube_stratum[number_of_period_at_risk_flare == 2 & level_of_pregnancy == 99, ub]]
    
    table_4_5[, npr_2 := pt_cube_stratum[number_of_period_at_risk_flare == 2 & level_of_pregnancy == 1, N_persons]]
    table_4_5[, mpr_2 := pt_cube_stratum[number_of_period_at_risk_flare == 2 & level_of_pregnancy == 1, flare]]
    table_4_5[, irpr_2 := pt_cube_stratum[number_of_period_at_risk_flare == 2 & level_of_pregnancy == 1, IR]]
    table_4_5[, llpr_2 := pt_cube_stratum[number_of_period_at_risk_flare == 2 & level_of_pregnancy == 1, lb]]
    table_4_5[, ulpr_2 := pt_cube_stratum[number_of_period_at_risk_flare == 2 & level_of_pregnancy == 1, ub]]
    
    table_4_5[, n_1_3 := pt_cube_stratum[number_of_period_at_risk_flare == 3 & level_of_pregnancy == 99, N_persons]]
    table_4_5[, m_3_0 := pt_cube_stratum[number_of_period_at_risk_flare == 3 & level_of_pregnancy == 99, flare]]
    table_4_5[, ir_3 := pt_cube_stratum[number_of_period_at_risk_flare == 3 & level_of_pregnancy == 99, IR]]
    table_4_5[, ll_3 := pt_cube_stratum[number_of_period_at_risk_flare == 3 & level_of_pregnancy == 99, lb]]
    table_4_5[, ul_3 := pt_cube_stratum[number_of_period_at_risk_flare == 3 & level_of_pregnancy == 99, ub]]
    
    table_4_5[, npr_3 := pt_cube_stratum[number_of_period_at_risk_flare == 3 & level_of_pregnancy == 1, N_persons]]
    table_4_5[, mpr_3 := pt_cube_stratum[number_of_period_at_risk_flare == 3 & level_of_pregnancy == 1, flare]]
    table_4_5[, irpr_3 := pt_cube_stratum[number_of_period_at_risk_flare == 3 & level_of_pregnancy == 1, IR]]
    table_4_5[, llpr_3 := pt_cube_stratum[number_of_period_at_risk_flare == 3 & level_of_pregnancy == 1, lb]]
    table_4_5[, ulpr_3 := pt_cube_stratum[number_of_period_at_risk_flare == 3 & level_of_pregnancy == 1, ub]]
    
    listvar1 <- grep("^n_1_", names(table_4_5), value = TRUE)
    listvar2 <- grep("^m_", names(table_4_5), value = TRUE)
    listvar3 <- grep("^npr_", names(table_4_5), value = TRUE)
    listvar4 <- grep("^mpr_", names(table_4_5), value = TRUE)
    listvar <- c(listvar1,listvar2,listvar3,listvar4)
    
    if (thisdatasource %in% datasources_stricter_masking) {
      
      if (stratum != "all" & thisdatasource %in% c("TEST", "DANREG")) {
        table_4_5 <- range_values_against_recalculation(table_4_5, listvar1)
        table_4_5 <- range_values_against_recalculation(table_4_5, listvar2)
        table_4_5 <- range_values_against_recalculation(table_4_5, listvar3)
        table_4_5 <- range_values_against_recalculation(table_4_5, listvar4)
      } else {
        table_4_5 <- mask_variables_per_threshold(table_4_5, listvar1, threshold[[thisdatasource]])
        table_4_5 <- mask_variables_per_threshold(table_4_5, listvar2, threshold[[thisdatasource]])
        table_4_5 <- mask_variables_per_threshold(table_4_5, listvar3, threshold[[thisdatasource]])
        table_4_5 <- mask_variables_per_threshold(table_4_5, listvar4, threshold[[thisdatasource]])
      }
      
      table_4_5 <- range_values_against_recalculation(table_4_5, c("n_1", "n_2"))
    } else {
      table_4_5 <- mask_variables_per_threshold(table_4_5, c(listvar, "n_2"), threshold[[thisdatasource]])
    }
    
    table_4_5[, threshold := threshold[[thisdatasource]]]
    
    if (stratum == "all") {
      fwrite(table_4_5, paste0(thisdiroutput, "D5_Table_4_IR_", immdis, ".csv"))
    } else {
      fwrite(table_4_5, paste0(thisdiroutput, "D5_Table_5_IR_", immdis, "_", stratum, ".csv"))
    }
    
    
  }
  
}