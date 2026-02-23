# PARAMETERS FOR POSTPROCESSING

localprocessing <- T



# datasources that have submitted data (only if localprocessing == F)

datasources_for_postprocessing <- c("BIFAP", "FISABIO", "SIDIAP", "PEDIANET", "SNDS", "UOSL","CPRD")
datasources_for_postprocessing <- c("BIFAP", "FISABIO", "SIDIAP", "PEDIANET", "SNDS", "UOSL", "FinReg", "CPRD")


datasources_with_flat_structure <- c( "CPRD","PEDIANET")

datasources_with_rounding <- c("FinReg")

# fill out the link to the submitted folders. they must be unzipped

folders_submission <- list()
folders_submission[["PEDIANET"]] <- "Z:/inbox/transfer-2025-03-11-14-56-luca.stona/"
folders_submission[["UOSL"]] <- "Z:/inbox/transfer-2025-03-12-13-31-mahmoud.zidan/g_export/Dummy tables/"
folders_submission[["FISABIO"]] <- "Z:/inbox/transfer-2025-03-12-09-08-juanjo.carreras/g_export/Dummy tables/"
folders_submission[["SIDIAP"]] <- "Z:/inbox/transfer-2025-03-12-08-45-carloalberto.bissacco/g_export_SIDIAP_122/Dummy tables/"
folders_submission[["SNDS"]] <- "Z:/inbox/transfer-2025-03-12-14-02-marie-agnes.bernard/g_export/Dummy tables/"
folders_submission[["CPRD"]] <- "Z:/inbox/transfer-2025-03-11-09-22-p.c.souverein/"
folders_submission[["BIFAP"]] <- "Z:/inbox/transfer-2025-03-13-08-24-patriciagpoza/g_export/Dummy tables/"
folders_submission[["FinReg"]] <- "Z:/inbox/transfer-2025-04-09-14-14-thuan.vo/uef_results_v1.2.2_20250330_entiredata/Dummy tables/"

# names of the data sources

name_ds <- list()

name_ds[["TEST"]] <- "Datasource TEST"
name_ds[["TEST2"]] <- "Datasource TEST2"
name_ds[["FISABIO"]] <- "VID"
name_ds[["PEDIANET"]] <- "PEDIANET"
name_ds[["BIFAP"]] <- "BIFAP"
name_ds[["SNDS"]] <- "SNDS"
name_ds[["SIDIAP"]] <- "SIDIAP"
name_ds[["UOSL"]] <- "NHR"
name_ds[["DANREG"]] <- "Danish registries"
name_ds[["CPRD"]] <- "CPRD"
name_ds[["FinReg"]] <- "Finnish registries"
name_ds[["EPICHRON"]] <- "EPICHRON"

# names of the immune diseases

name_immdis <- list()

name_immdis[["E_GRAVES_AESI"]] <- "Graves' disease"
name_immdis[["Im_HASHIMOTO_AESI"]] <- "Hashimoto's thyroiditis"
name_immdis[["V_PAN_AESI"]] <- "Polyarteritis nodosa"
name_immdis[["M_ARTRHEU_AESI"]] <- "Rheumatoid arthritis"
name_immdis[["M_ARTPSORIATIC_AESI"]] <- "Psoriatic arthritis"
name_immdis[["N_DEMYELMS_AESI"]] <- "Multiple sclerosis"
name_immdis[["SK_ERYTHEMANODOSUM_AESI"]] <- "Erythema nodosum"
name_immdis[["Im_SLE_AESI"]] <- "Systemic lupus erythematosus"
name_immdis[["D_ULCERATIVECOLITIS_AESI"]] <- "Ulcerative colitis"
name_immdis[["D_HEPATITISAUTOIMMUNE_AESI"]] <- "Autoimmune hepatitis"
name_immdis[["Im_HASHIMOTO_AESI_sensitivity"]] <- "Hashimoto's thyroiditis (sensitivity)"
name_immdis[["N_DEMYELMS_AESI_sensitivity"]] <- "Multiple sclerosis (sensitivity)"

# parameter file for shell tables

File_SAFETY_VAC_shell_tables <- paste0(thisdir, "/p_parameters/archive_parameters/parameters_ShellTables.xlsx")

# names of covariates and covariates of each immune disease
name_of_cov_tab <- as.data.table(readxl::read_excel(File_SAFETY_VAC_shell_tables, sheet = "covariates_param"))

clean_name_of_cov_tab <- name_of_cov_tab[!is.na(parameter_value) & length(parameter_value) > 0,]

name_cov <- as.list(setNames(clean_name_of_cov_tab$covariate_lab, clean_name_of_cov_tab$parameter_value))

cov_of_immdis <- clean_name_of_cov_tab[, list(parameter_value = list(parameter_value)), by = immdis]
cov_of_immdis <- setNames(cov_of_immdis$parameter_value, cov_of_immdis$immdis)
cov_of_immdis[["Im_HASHIMOTO_AESI_sensitivity"]] <- cov_of_immdis[["Im_HASHIMOTO_AESI"]]
cov_of_immdis[["N_DEMYELMS_AESI_sensitivity"]] <- cov_of_immdis[["N_DEMYELMS_AESI"]]

# names of groups of prompts

name_group <- list()
name_group[["PC"]] <- "PC"
name_group[["HOSP_DISP"]] <- "Inpatient"
name_group[["HOSP_SPEC_DISP"]] <- "Inpatient or specialist"

# threshold for masking

threshold <- list()
threshold[[thisdatasource]] <- 5

# immune diseases where ablation analysis does not apply to UOSL

immdis_UOSL_no_ablation <- c("M_ARTPSORIATIC_AESI","SK_ERYTHEMANODOSUM_AESI","D_ULCERATIVECOLITIS_AESI","D_HEPATITISAUTOIMMUNE_AESI")
immdis_UOSL_no_ablation <- c()

# datasources that require to remove higher level counts if only one small count exists

ds_rm_higher_level_if_one_smallcount <- c("UOSL","DANREG")

datasources_stricter_masking <- c("FinReg", "DANREG", "TEST")
