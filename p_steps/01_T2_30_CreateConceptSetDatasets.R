##%######################################################%##
#                                                          #
#### EXTRACT FROM CDM TABLES ONE DATASET PER CONCEPTSET ####
####  CONTAINING ONLY RECORDS WITH A CODE OF INTEREST   ####
#                                                          #
##%######################################################%##

events_to_restrict <- c(conceptsets_exact_matching, conceptsets_children_matching)

if (thisdatasource %in% c("FinReg")) {
  events_to_restrict <- c(conceptsets_exact_matching, conceptsets_children_matching)[grepl("^TP_", unique(c(conceptsets_exact_matching, conceptsets_children_matching)))]
}

if (thisdatasource %in% c("FISABIO")) {
  events_to_restrict <- intersect(concept_sets_of_our_study, c(paste0(VACCOID_list,"ATC"), paste0(VACCOID_list,"MED"), paste0(VACCOID_list,"VXTYPE")))
}

# if (thisdatasource %in% c("SNDS")) {
#   all_concepts <- unique(c(conceptsets_exact_matching, conceptsets_children_matching))
#   events_to_restrict <- c(all_concepts[grepl("^DP_DILIMOST", all_concepts)], all_concepts[grepl("^DP_ANALGESIC", all_concepts)])
# }
# 
# if (thisdatasource %in% c("CPRD")) {
#   all_concepts <- unique(c(conceptsets_exact_matching, conceptsets_children_matching))
#   events_to_restrict <- c(all_concepts[grepl("^DP_DILIMOST", all_concepts)], all_concepts[grepl("^DP_ANALGESIC", all_concepts)], all_concepts[grepl("^DP_COVCARDIOCEREBROVAS", all_concepts)], all_concepts[grepl("^DP_VACCINES", all_concepts)], all_concepts[grepl("^COVID_VACCINES", all_concepts)], all_concepts[grepl("^DP_IMMUNOSUPRESSANTSONLYPAN", all_concepts)], all_concepts[grepl("^DP_NSAIDRA", all_concepts)], all_concepts[grepl("^DP_TNFEN", all_concepts)])
# }
# 
# if (thisdatasource %in% c("FinReg")) {
#   events_to_restrict <- c(unique(unlist(flare_components)))
# }

events_to_restrict <- c(events_to_restrict, paste0(events_to_restrict, "_narrow"), paste0(events_to_restrict, "_possible"))

conceptsets_exact_matching <- intersect(events_to_restrict, conceptsets_exact_matching)
conceptsets_children_matching <- intersect(events_to_restrict, conceptsets_children_matching)

print('RETRIEVE FROM CDM RECORDS CORRESPONDING TO CONCEPT SETS')

# Create filter expression
used_domains <- unique(unlist(concept_set_domains))
if (thisdatasource == "BIFAP") {
  filter_expression <- rep(list("data.table::between(date, start_lookback, study_end)"), length(used_domains))
} else {
  filter_expression <- rep(list("T"), length(used_domains))
}
names(filter_expression) <- used_domains

CreateConceptSetDatasets(concept_set_names = conceptsets_exact_matching,
                         dataset = ConcePTION_CDM_tables,
                         codvar = ConcePTION_CDM_codvar,
                         datevar = ConcePTION_CDM_datevar,
                         EAVtables = ConcePTION_CDM_EAV_tables,
                         EAVattributes = ConcePTION_CDM_EAV_attributes_this_datasource,
                         dateformat= "YYYYmmdd",
                         vocabulary = ConcePTION_CDM_coding_system_cols,
                         rename_col = list(person_id = person_id, date = date,
                                           meaning_renamed = meaning_renamed),
                         filter_expression = filter_expression,
                         concept_set_domains = concept_set_domains,
                         concept_set_codes =	concept_set_codes_our_study,
                         concept_set_codes_excl = concept_set_codes_our_study_excl,
                         discard_from_environment = T,
                         dirinput = dirinput,
                         diroutput = dirconceptsets,
                         extension = c("csv"),
                         vocabularies_with_dot_wildcard = c("READ"),
                         vocabularies_with_exact_search_not_dot = c("Free_text", "ICD10CM", "ICD10GM", "ICD10", "ICD9CM",
                                                                    "ICD9", "ICPC", "ICPC2P", "SNOMED", "MEDCODEID"))

CreateConceptSetDatasets(concept_set_names = conceptsets_children_matching,
                         dataset = ConcePTION_CDM_tables,
                         codvar = ConcePTION_CDM_codvar,
                         datevar = ConcePTION_CDM_datevar,
                         EAVtables = ConcePTION_CDM_EAV_tables,
                         EAVattributes = ConcePTION_CDM_EAV_attributes_this_datasource,
                         dateformat= "YYYYmmdd",
                         vocabulary = ConcePTION_CDM_coding_system_cols,
                         rename_col = list(person_id = person_id, date = date,
                                           meaning_renamed = meaning_renamed),
                         concept_set_domains = concept_set_domains,
                         concept_set_codes =	concept_set_codes_our_study,
                         concept_set_codes_excl = concept_set_codes_our_study_excl,
                         discard_from_environment = T,
                         dirinput = dirinput,
                         diroutput = dirconceptsets,
                         extension = c("csv"),
                         vocabularies_with_dot_wildcard = c("READ"),
                         vocabularies_with_exact_search = c("Free_text"))
