#-------------------------------
# SAFETY-VAC Objective 2 script 

# authors: Rosa Gini, Davide Messina, Belen Castillo Cano

# CHANGELOG

# v 1.2.6 - 08 August 2025
# Improved masking for DANREG

# v 1.2.5 - 18 July 2025
# Fix bug for possible flare windows
# Fixed KM figure if dataset is missing
# Fixed minor bug in masking and postprocessing
# Creation of concepts now works even without PROCEDURES table

# v 1.2.4 - 28 May 2025
# Improved masking for D5 and D6

# v 1.2.3 - 30 April 2025
# Updated codelist with ICD10DA
# Create missing folder
# Move postprocess tables to g_postprocess to not be uploaded
# Updated vaccines metadata
# Fixed imputation of birth date

# v 1.2.2 - 10 March 2025
# Fixed cohort selection for N_DEMYELMS_AESI sensitivity
# Fixed table 1 of postprocessing

# v 1.2.1 - 07 March 2025
# Stricter masking for some FinReg and DANREG

# v 1.2.0 - 05 March 2025
# Disaggregated criteria in table 1
# Fix additional exclusion criteria for cohorts 
# Update D6 creation and Summary document
# Flowcharts now only in g_output
# Added two sensitivity cohorts

# v 1.1.3 - 15 January 2025
# Added missing ICD10DA

# v 1.1.2 - 21 November 2024
# Fixed table 3
# Table8: Update meanings of events for some DAPs

# v 1.1.1 - 23 October 2024
# Fixed parameter to retrieve dates for procedures in correct format
# More robust component creation
# Int64 fix for createconceptsetdataset
# Splitting creation of complex algorihm for BIFAP
# Fixed meanings for SNDS

# v 1.1.0 - 23 October 2024
# Fixed table 1 final criteria
# Added pregnancies in tables 2 and 3
# Added procedure codes for nordic countries and duplicate ICD10PCS
# Increase robustness of concepts creation
# Fixed ageband IRs in tables 4-5
# Covariates calculated until t0 -1
# Fixed group of prompts of CPRD and SNDS
# Added missing meanings for UOSL

# v 1.0.5 - 18 October 2024
# Codelist now include ICPC
# No changes to coding system from the codelist (except ICPC)

# v 1.0.4 - 14 October 2024
# Fixed bug in creation of table 8
# Improved memory usage of complex algorithm
# More robust importation of concepts in 03_20

# v 1.0.3 - 10 October 2024
# Persontime cube to keep only pregnant and total
# Increased robustness for splitting concepts
# Concepts restriction for FinReg

# v 1.0.2 - 08 October 2024
# Fixed start followup in case of multiple vax spells
# Modified postprocessing of table 6

# v 1.0.1 - 06 October 2024
# Corrected group of prompt for DANREG, UOSL and FinReg
# Finished postprocessing of table 6

# v 1.0.0 - 04 October 2024
# More robust splitting of concepts
# Fixed persontime in intermediate datasets
# Masking of final tables

# v 0.2.4 - 03 October 2024
# Fixed pregnancy persontime and final tables
# Fixed cohort creation
# Updated BRIDGE
# Use correct domains

# v 0.2.3 - 03 October 2024
# Fixed cohort entrance criteria
# New/modified cube tables
# Fixed naming of variables for table 3
# Table 4 and 5
# Partial postprocessing

# v 0.2.2 - 02 October 2024
# Added filter during creation of the concepts to speed up the process

# v 0.2.1 - 01 October 2024
# Fixed codelist

# v 0.2.0 - 01 October 2024
# Testing of the second part of the script excl table 4,5 and KM

# v 0.1.1 - 30 September 2024
# Split DP_COVCARDIOCEREBROVAS concept for SNDS

# v 0.1.0 - 20 September 2024
# Initial release of first part of Obj. 2
# Fixed meaning selection
# Fixed vax and flare windows

# v 0.0.1 - 12 September 2024
# First commit based on Obj 2

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

##%######################################################%##
#                                                          #
####                     PARAMETERS                     ####
#                                                          #
##%######################################################%##

source(paste0(thisdir,"/p_parameters/01_parameters_program.R"))
source(paste0(thisdir,"/p_parameters/02_parameters_CDM.R"))
source(paste0(thisdir,"/p_parameters/03_concept_sets.R"))
source(paste0(thisdir,"/p_parameters/04_itemsets.R"))
source(paste0(thisdir,"/p_parameters/05_subpopulations_restricting_meanings.R"))
source(paste0(thisdir,"/p_parameters/06_variable_lists.R"))
source(paste0(thisdir,"/p_parameters/07_algorithms.R"))
source(paste0(thisdir,"/p_parameters/08_parameters_postprocessing.R"))
source(paste0(thisdir,"/p_parameters/11_design_parameters.R"))
source(paste0(thisdir,"/p_parameters/99_saving_all_parameters.R"))

##%######################################################%##
#                                                          #
####                    MAIN SCRIPT                     ####
#                                                          #
##%######################################################%##

launch_step("p_steps/01_T2_10_create_persons.R")
launch_step("p_steps/01_T2_20_apply_CreateSpells.R")
launch_step("p_steps/01_T2_30_CreateConceptSetDatasets.R")
launch_step("p_steps/01_T2_40_clean_vaccines.R")
launch_step("p_steps/01_T2_41_apply_criteria_for_doses.R")
launch_step("p_steps/01_T2_42_clean_all_vaccines.R")
launch_step("p_steps/01_T2_43_curate_all_vaccines.R")
launch_step("p_steps/01_T2_50_clean_spells.R")
launch_step("p_steps/01_T2_60_selection_criteria_from_PERSON_to_source_population.R")
launch_step("p_steps/02_T3_10_create_source_population.R")

launch_step("p_steps/03_T2_10_create_cohort.R")
launch_step("p_steps/03_T2_11_create_cohort_flowcharts.R")
launch_step("p_steps/03_T2_20_create_components_flares.R")
launch_step("p_steps/03_T2_30_create_dates_flares.R")
launch_step("p_steps/03_T2_40_create_D3_outcomes_complex_algorithm.R")
launch_step("p_steps/03_T2_50_create_TD_datasets.R")
launch_step("p_steps/03_T2_60_create_periods_followup.R")

launch_step("p_steps/03_T2_70_create_pregnancies_during_periods_followup.R")
launch_step("p_steps/03_T2_71_TD_pregnancy_during_followup.R")
launch_step("p_steps/03_T2_80_create_periods_followup_with_pregnancies.R")

launch_step("p_steps/04_T3_10_create_persontime.R")
launch_step("p_steps/04_T3_11_create_persontime_Cube.R")
launch_step("p_steps/04_T3_12_create_population_Cube.R")
launch_step("p_steps/04_T3_20_create_analytical_dataset_KM.R")
launch_step("p_steps/04_T3_30_create_component_dataset.R")

launch_step("p_steps/05_T4_10_Table_1_Attrition.R")
launch_step("p_steps/05_T4_20_Table_2_Cohort_characteristics.R")
launch_step("p_steps/05_T4_30_Table_3_Covariates.R")
launch_step("p_steps/05_T4_40_Table_4_5_IR.R")
launch_step("p_steps/05_T4_50_Table_6_components_flares.R")
launch_step("p_steps/05_T4_60_KM_analysis.R")
launch_step("p_steps/05_T4_70_Table_8_impact_of_ablation_cohort.R")

launch_step("p_steps/06_T5_10_Table_1_Attrition.R")
launch_step("p_steps/06_T5_20_Table_2_Cohort_characteristics.R")
launch_step("p_steps/06_T5_30_Table_3_Covariates.R")
launch_step("p_steps/06_T5_40_Table_4_IR.R")
launch_step("p_steps/06_T5_50_Table_5_IR_age_gender.R")
launch_step("p_steps/06_T5_60_Table_6_components_flares.R")
launch_step("p_steps/06_T5_70_Table_7_Cumulative_incidence.R")
launch_step("p_steps/06_T5_71_Figure_KM.R")
launch_step("p_steps/06_T5_80_Table_8_Impact_of_ablation.R")
launch_step("p_steps/06_T5_81_Fig_ablation.R")

launch_step("p_steps/07_T5_10_SummaryDocument.R")

launch_step("p_steps/99_10_check_intermediate_small.R")
