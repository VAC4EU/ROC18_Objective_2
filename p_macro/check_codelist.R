metadata <- here::here("p_parameters", "archive_parameters", "ADEPT_O1_BRIDGE_19Mayo25.xlsx")
codelist <- here::here("p_parameters", "archive_parameters", "20241022_ROC18_codelist_with procedures_at_20250319.csv")
output_codelist_name <- here::here("p_parameters", "archive_parameters", "20241022_ROC18_codelist_with_procedures_at_20250610.csv")

`%not in%` <- Negate(`%in%`)

output_folder <- here::here("p_parameters", "archive_parameters", "codelist check",
                            base::format(.POSIXct(Sys.time(), "GMT"), format = "%Y%m%dT%H%M%S"))
dir.create(output_folder, recursive = T)

VAR_list <- data.table::as.data.table(readxl::read_excel(metadata, sheet = "OBJ1"))
VAR_list <- VAR_list[rowSums(VAR_list[, c("EXPOSURE", "COV", "INDICATION", "ALTERNATIVES", "Algorithm_input")]) > 0]
if (nrow(VAR_list[xor(is.na(Type_codelist), is.na(EXACT_MATCH)), ]) > 0) {
  warning("BRIDGE files contains a Varname with only one of column between Type_codelist and EXACT_MATCH missing")
}
VAR_list <- VAR_list[!is.na(Type_codelist), ]
VAR_list[, Varname := trimws(Varname)]
VAR_list[, c("system", "event_abbreviation", "type") := data.table::tstrsplit(Varname, "_", fixed = T)]
VAR_list[, Varname := NULL]

OUT_codelist <- data.table::fread(codelist)

original_order <- colnames(OUT_codelist)

test_codelist <- unique(OUT_codelist[, .(system, event_abbreviation, type)])
test_VAR_list <- unique(VAR_list[, .(system, event_abbreviation, type)])
test_VAR_list <- test_VAR_list[system != "DP", ]
var_in_metadata_no_codes <- setdiff(test_VAR_list[, paste(system, event_abbreviation, type, sep = "_")],
                                    paste(test_codelist$system, test_codelist$event_abbreviation, test_codelist$type, sep = "_"))

exported_var_in_metadata_no_codes <- if (identical(var_in_metadata_no_codes, character(0))) {
  "OK: all variables in the codelist"
} else {var_in_metadata_no_codes}
data.table::fwrite(as.list(exported_var_in_metadata_no_codes), here::here(output_folder, "missing variables.csv"))
if (length(var_in_metadata_no_codes) > 0) {warning(paste("There are some variables not in the codelist:",
                                                         paste(var_in_metadata_no_codes, collapse = ", ")))}

OUT_codelist <- merge(VAR_list, OUT_codelist, by = c("system", "event_abbreviation", "type"), allow.cartesian = F)
data.table::setcolorder(OUT_codelist, original_order)

data.table::fwrite(OUT_codelist, output_codelist_name)

# Check rounding errors
round_0 <- unique(OUT_codelist$code[grepl("(?=.*\\.)(?=.*00000)", OUT_codelist$code, perl = T)])
round_9 <- unique(OUT_codelist$code[grepl("(?=.*\\.)(?=.*99999)", OUT_codelist$code, perl = T)])

data.table::fwrite(data.table::copy(OUT_codelist)[code %in% c(round_0, round_9), ],
                   here::here(output_folder, "rounding_errors.csv"))
OUT_codelist <- OUT_codelist[code %not in% c(round_0, round_9)]

if (length(round_0) > 0) {warning(paste("There are some codes with rounding issues:", paste(round_0, collapse = ", ")))}
if (length(round_9) > 0) {warning(paste("There are some codes with rounding issues:", paste(round_9, collapse = ", ")))}

# Check for special characters
codes_special_char <- gsub("[0-9]|[a-z]|[A-Z]|\\.", "", OUT_codelist$code, perl = T)
codes_special_char <- OUT_codelist$code[codes_special_char != ""]
codes_special_char_narrow <- codes_special_char[grepl("_narrow", codes_special_char)]
codes_special_char_broad <- codes_special_char[grepl("_broad", codes_special_char)]
codes_special_char_possible <- codes_special_char[grepl("_possible", codes_special_char)]
codes_special_char <- codes_special_char[!(grepl("_narrow", codes_special_char) | grepl("_broad", codes_special_char) | grepl("_possible", codes_special_char))]
codes_special_char <- unique(codes_special_char)

data.table::fwrite(data.table::copy(OUT_codelist)[code %in% codes_special_char, ],
                   here::here(output_folder, "special_characters.csv"))

OUT_codelist <- OUT_codelist[code %not in% codes_special_char]

if (length(codes_special_char) > 0) {warning(paste("There are some codes with special characters:",
                                                   paste(codes_special_char, collapse = ", ")))}

# OUT_codelist_recoded <- data.table::copy(OUT_codelist)[, coding_system := data.table::fcase(
#   coding_system %in% c("ICD10", "ICD10CM", "ICD10PCS", "ICD10PC"), "ICD10CM",
#   coding_system %in% c("ICD10DA"), "ICD10DA",
#   coding_system %in% c("Free_text"), "Free_text",
#   coding_system %in% c("free_text"), "free_text",
#   coding_system %in% c("ICD9CM", "ICD9", "ICD9CM_HSD", "ICD9CMP"), "ICD9CM",
#   coding_system %in% c("MTHICD9"), "MTHICD9",
#   coding_system %in% c("ICPC2P", "ICPC2EENG", "ICPC2", "ICPC"), "ICPC2P",
#   coding_system %in% c("RCD2", "RCD"), "READ",
#   coding_system %in% c("SCTSPA", "SNOMEDCT_US", "SPA_EXT", "SNM", "SCTSPA_SNS", "SNOMED", "MDR", "HCPCS", "HCPT",
#                        "HSD_ACCERT", "NCMPNCSPNCRP"), "SNOMED",
#   coding_system %in% c("MEDCODEID", "MEDCODE"), "MEDCODEID",
#   coding_system %in% c("ATC"), "ATC"
# )]
# 
# strange_coding_system <- OUT_codelist[is.na(OUT_codelist_recoded$coding_system), ]
# data.table::fwrite(strange_coding_system, here::here("p_parameters", "archive_parameters", "strange_coding_systems.csv"))
# 
# if (nrow(strange_coding_system) > 0) {warning(paste("There are some coding systems not defined:",
#                                                     paste(unique(strange_coding_system$coding_system), collapse = ", ")))}
# 
# OUT_codelist <- unique(OUT_codelist[!is.na(OUT_codelist_recoded$coding_system)])

# Check for inconsistent tagging
inconsistent_tagging <- data.table::copy(OUT_codelist)[, .(coding_system, code, system, event_abbreviation, type, tags)]
inconsistent_tagging <- unique(inconsistent_tagging)
inconsistent_tagging <- inconsistent_tagging[type == "AESI", ]
inconsistent_tagging <- inconsistent_tagging[, .N, by = c("coding_system", "code", "system",
                                                          "event_abbreviation", "type")]
inconsistent_tagging <- inconsistent_tagging[N > 1,][, N := NULL]

if (nrow(inconsistent_tagging) > 0) {warning(paste("There are some codes with inconsistent tagging:",
                                                   paste(unique(inconsistent_tagging$code), collapse = ", ")))}

data.table::fwrite(inconsistent_tagging, here::here(output_folder, "inconsistent_tagging.csv"))

# OUT_codelist[grepl("^[0-9]{3}\\.[0-9]{3}", code, perl = T)]

# Verify if at least one narrow, possible and exclude are present
summary_tags <- OUT_codelist[, .N, by = c("tags")]
exist_narrow <- "narrow" %in% summary_tags$tags
exist_possible <- "possible" %in% summary_tags$tags
exist_exclude <- "exclude" %in% summary_tags$tags

if (!exist_narrow) {warning("There are no narrow codes")}
if (!exist_possible) {warning("There are no possible codes")}
if (!exist_exclude) {warning("There are no exclude codes")}

# Verify if tags outside usual names are present
exist_strange <- summary_tags$tags[summary_tags$tags %not in% c("narrow", "possible", "exclude")]

if (length(exist_strange) > 0) {warning(paste("There are not approved tags:", exist_strange))}
data.table::fwrite(data.table::data.table(strange_tags = exist_strange), here::here(output_folder, "exist_strange_tags.csv"))

# # Verify Free_text vocabulary
# free_text <- OUT_codelist[coding_system == "Free_text"]
# free_text[, tag_in_code := data.table::tstrsplit(code, "_", fixed = T)[2]]
# free_text[, event_in_code := data.table::tstrsplit(code, "_", fixed = T)[1]]
# free_text <- data.table::dcast(free_text, coding_system + type + event_abbreviation + event_in_code + system ~ tag_in_code,
#                                fun.aggregate =  function(x) as.logical(min(length(x), 1)), value.var = "tag_in_code")
# 
# exist_possible_not_broad <- free_text[broad == F & possible == T, ]
# exist_broad_not_possible <- free_text[broad == T & possible == F, ]
# exist_possible_not_narrow <- free_text[(broad == T | possible == T) & narrow == F, ]
# exist_narrow_not_possible <- free_text[broad == F & possible == F & narrow == T, ]
# 
# if (nrow(exist_possible_not_broad) > 0) {warning(paste("There are free text possible codes without broad:",
#                                                        paste(unique(exist_possible_not_broad$code), collapse = ", ")))}
# if (nrow(exist_broad_not_possible) > 0) {warning(paste("There are free text broad codes without possible:",
#                                                        paste(unique(exist_broad_not_possible$code), collapse = ", ")))}
# if (nrow(exist_possible_not_narrow) > 0) {warning(paste("There are free text possible codes without narrow:",
#                                                         paste(unique(exist_possible_not_narrow$code), collapse = ", ")))}
# if (nrow(exist_narrow_not_possible) > 0) {warning(paste("There are free text narrow codes without possible:",
#                                                         paste(unique(exist_narrow_not_possible$code), collapse = ", ")))}
# 
# data.table::fwrite(exist_possible_not_broad, here::here(output_folder, "exist_possible_not_broad.csv"))
# data.table::fwrite(exist_broad_not_possible, here::here(output_folder, "exist_broad_not_possible.csv"))
