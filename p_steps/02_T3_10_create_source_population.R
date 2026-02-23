##%######################################################%##
#                                                          #
####          APPLY GENERAL EXCLUSION CRITERIA          ####
####             TO CREATE STUDY POPULATION             ####
#                                                          #
##%######################################################%##

print('FLOWCHART')

# USE THE FUNCTION CREATEFLOWCHART TO SELECT THE SUBJECTS IN POPULATION

for (subpop in subpopulations_non_empty){
  
  # Create flowchart for adults and save D4_study_population
  load(paste0(dirtemp,"D3_selection_criteria_from_PERSONS_to_source_population.RData"))
  selection_criteria <- get(paste0("D3_selection_criteria_from_PERSONS_to_source_population"))
  
  selected_population <- CreateFlowChart(
    dataset = selection_criteria,
    listcriteria = c("sex_or_birth_date_is_not_defined", "partial_date_of_death", "birth_date_absurd", "no_spells",
                     "all_spells_start_after_ending", "no_spell_longer_than_365_days",
                     "no_spell_overlapping_the_study_period"),
    flowchartname = "Flowchart_exclusion_criteria")
  
  fwrite(Flowchart_exclusion_criteria, paste0(diroutput, "Flowchart_exclusion_criteria.csv"))
  
  selected_population <- data.table::as.data.table(selected_population)
  selected_population <- selected_population[, .(person_id, birth_date, death_date, sex_at_instance_creation,
                                                 spell_start_date, study_entry_date, study_exit_date)]
  
  saveRDS(selected_population, file = paste0(diroutput, "D4_source_population.rds"))
}
