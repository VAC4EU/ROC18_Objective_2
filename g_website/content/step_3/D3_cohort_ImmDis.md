---
weight: 1
name_excel: "D3_cohort_ImmDis.xlsx"
description: ""
slug: "D3_cohort_ImmDis"
datetime: 1.7268637e+09
title: D3_cohort_ImmDis
author: ''
date: '2024-09-20'
categories: []
tags: []
archetype: codebook
output: html_document
---

<script src="/rmarkdown-libs/core-js/shim.min.js"></script>
<script src="/rmarkdown-libs/react/react.min.js"></script>
<script src="/rmarkdown-libs/react/react-dom.min.js"></script>
<script src="/rmarkdown-libs/reactwidget/react-tools.umd.cjs"></script>
<script src="/rmarkdown-libs/htmlwidgets/htmlwidgets.js"></script>
<link href="/rmarkdown-libs/reactable/reactable.css" rel="stylesheet" />
<script src="/rmarkdown-libs/reactable-binding/reactable.js"></script>
<div class="tab">
<button class="tablinks" onclick="openCity(event, &#39;Metadata&#39;)" id="defaultOpen">Metadata</button>
<button class="tablinks" onclick="openCity(event, &#39;Data Model&#39;)">Data Model</button>
<button class="tablinks" onclick="openCity(event, &#39;Parameters&#39;)">Parameters</button>
<button class="tablinks" onclick="openCity(event, &#39;Example&#39;)">Example</button>
</div>
<div id="Metadata" class="tabcontent">
<div id="htmlwidget-1" class="reactable html-widget" style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-1">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"metadata_name":["Name of the dataset","Content of the dataset","Unit of observation","Dataset where the list of UoOs is fully listed and with 1 record per UoO","How many observations per UoO","NxUoO","Variables capturing the UoO","Primary key","Parameters",null,null,null,null,null,null,null,null,null,null,null],"metadata_content":["D3_cohort_{ImmDis}","Persons in the source population that have at least a code of {ImmDis} during study period, with selection crieria and date of entrance and start of followup in the cohort of each group of prompts","persons in the source population that have at least a code of {ImmDis} during the study petiod","itself","1","1","person_id","person_id","ImmDis",null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"metadata_name","name":"metadata_name","type":"character"},{"id":"metadata_content","name":"metadata_content","type":"character"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"de3af57700fcf524b9897d94f41aa151"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Data Model" class="tabcontent">
<div id="htmlwidget-2" class="reactable html-widget" style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-2">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"Varname":["person_id","study_entry_date","study_exit_date","has_not_a_code_in_the_study_period_{ImmDis}","exclude_because_exist_code_during_lookback_{ImmDis}","exclude_because_exist_exclusion_criterion_during_lookback_{ImmDis}","enter_cohort_{ImmDis}","cohort_entry_date_{ImmDis}","cause_for_not_entering_followup_{ImmDis}","entering_follow_up_postponed_{ImmDis}","entering_follow_up_{ImmDis}","start_follow_up_{ImmDis}","has_a_code_in_the_study_period_{ImmDis}_{group_of_prompt}","exclude_because_exist_code_during_lookback_{ImmDis}_{group_of_prompt}","exclude_because_exist_exclusion_criterion_during_lookback_{ImmDis}_{group_of_prompt}","enter_cohort_{ImmDis}_{group_of_prompt}","cohort_entry_date_{ImmDis}_{group_of_prompt}","cause_for_not_entering_followup_{ImmDis}_{group_of_prompt}","entering_follow_up_postponed_{ImmDis}_{group_of_prompt}","entering_follow_up_{ImmDis}_{group_of_prompt}"],"Description":[null,"start of the study period","end of the observation period","whether there is at least a code of {ImmDis} during the study period. Having this variable = 1 is the inclusion criterion in the UoOs of this dataset","during lookback there is a code of {ImmDis} (not an incident case)","additional exclusion criterion (presence during lookback of some treatments and/or diagnostic codes)","entrance in the cohort of {ImmDis}","date when the person is first found with {ImmDis}","reason why the persons does not enter follow up (if any)","if the person is vaccinated during the first 90 days after cohort_entry_date, then their entry is potsponed to 90 days after the vaccination (if vaccinated more than once in a 90-days window, the computation of 90 days is restarted)","whether the person enters follow up for {ImmDis}","date when the person enters the follow up for {ImmDis}","whether there is at least a code of {ImmDis} during the study period when restricted to {group_of_prompt}","during lookback there is a code of {ImmDis} (not an incident case)","additional exclusion criterion (presence during lookback of some treatments and/or diagnostic codes)","entrance in the cohort of {ImmDis}","date when the person is first found for {ImmDis} in the study period when the prompts are restricted to {group_of_prompt}","reason why the persons does not enter follow up (if any)  when the prompts are restricted to {group_of_prompt}","if the person is vaccinated during the first 90 days after cohort_entry_date, then  their entry is potsponed to 90 days after the vaccination (if vaccinated more than once in a 90-days window, the computation of 90 days is restarted)","whether the person enters follow up of {ImmDis} when the prompts are restricted to {group_of_prompt}"],"Format":[null,"date",null,"int","int","int","int",null,"int","int","int",null,"int","int","int","int",null,"int","int",null],"Vocabulary":[null,null,null,"0 = Not exclude\r\n1 = Exclude","0 = Not exclude\r\n1 = Exclude","0 = Not exclude\r\n1 = Exclude","0 = No\r\n1 = Yes",null,"0  = no reason, person enters follow-up\r\n1 = person exits the data source before 90 days while still alive\r\n2 = person dies before 90 days\r\n3 = …","0 = No\r\n1 = Yes","0 = No\r\n1 = Yes",null,"0 = Not exclude\r\n1 = Exclude","0 = Not exclude\r\n1 = Exclude","0 = Not exclude\r\n1 = Exclude","0 = No\r\n1 = Yes",null,"0  = no reason, person enters follow-up\r\n1 = person exits the data source before 90 days while still alive\r\n2 = person dies before 90 days\r\n3 = …\r\n9 = the person has not entered the cohort with when the prompts are restricted to {group_of_prompt}","0 = No\r\n1 = Yes",null],"Description / Notes":[null,null,null,null,null,"this is only defined for few values of {ImmDis}, see corresponding BRIDGE",null,null,null,null,null,null,null,null,"this is only defined for few values of {ImmDis}, see corresponding BRIDGE",null,null,null,null,null],"Parameters":[null,null,null,null,null,null,null,"ImmDis","ImmDis","ImmDis","ImmDis","ImmDis",null,null,null,null,"ImmDis group_of_prompt","ImmDis group_of_prompt","ImmDis group_of_prompt","ImmDis group_of_prompt"],"Source tables and variables":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Retrieved":["yes","yes","yes",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Calculated":[null,null,null,"yes",null,null,null,"yes","yes","yes","yes","yes","yes",null,null,null,null,null,null,null],"Algorithm_id":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Rule":[null,null,null,"exists code of {ImmDis} with date >= study_entry_date\r\n","exists code of {ImmDis} with date < study_entry_date then 1\r\n0 otherwise\r\n","only for selected diseases: exists code of additional exclusion criterion with date < study_entry_date and date >= study_entry_date - 365 then 1\r\n0 otherwise\r\n",null,null,null,"cohort_entry_date_{ImmDis} & if cause_for_not_entering_followup_{ImmDis} == 0 and exists vaccine_date >  cohort_entry_date_{ImmDis} & vaccine_date <= cohort_entry_date_{ImmDis} +90 then 1\r\n0 otherwise\r\n",null,"if  entering_follow_up_{ImmDis} == 1 & entering_follow_up_postponed_{ImmDis} == 0:\r\ncohort_entry_date_{ImmDis} + 90\r\notehwrsie if entering_follow_up_{ImmDis} == 1 & entering_follow_up_postponed_{ImmDis} == 1:\r\ndate of last vasscine in window of 90 days + 90\r\notherwise: null","exists code of {ImmDis} with date >= study_entry_date\r\n","exists code of {ImmDis} with date < study_entry_date then 1\r\n0 otherwise\r\n","only for selected diseases: exists code of additional exclusion criterion with date < study_entry_date and date >= study_entry_date - 365 then 1\r\n0 otherwise\r\n",null,null,null,null,null]},"columns":[{"id":"Varname","name":"Varname","type":"character"},{"id":"Description","name":"Description","type":"character"},{"id":"Format","name":"Format","type":"character"},{"id":"Vocabulary","name":"Vocabulary","type":"character"},{"id":"Description / Notes","name":"Description / Notes","type":"character"},{"id":"Parameters","name":"Parameters","type":"character"},{"id":"Source tables and variables","name":"Source tables and variables","type":"logical"},{"id":"Retrieved","name":"Retrieved","type":"character"},{"id":"Calculated","name":"Calculated","type":"character"},{"id":"Algorithm_id","name":"Algorithm_id","type":"logical"},{"id":"Rule","name":"Rule","type":"character"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"ced7b137dfa1d8521c659f0ff08487c3"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Parameters" class="tabcontent">
<div id="htmlwidget-3" class="reactable html-widget" style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-3">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"parameter":["ImmDis","ImmDis","ImmDis","ImmDis","ImmDis","ImmDis","ImmDis","ImmDis","ImmDis","ImmDis","group_of_prompt","group_of_prompt","group_of_prompt",null,null,null,null,null,null,null],"value":["E_GRAVES_AESI","Im_HASHIMOTO_AESI","V_PAN_AESI","M_ARTRHEU_AESI","M_ARTPSORIATIC_AESI","N_DEMYELMS_AESI","SK_ERYTHEMANODOSUM_AESI","Im_SLE_AESI","D_ULCERATIVECOLITIS_AESI","D_HEPATITISAUTOIMMUNE_AESI","PC","HOSP_DISP","HOSP_SPEC_DISP","…","…","…","…",null,null,"…"],"label":["Graves","Hashimoto","Polyarteritis nodose","Rheumatoid arthritis","Psoriatic arthritis","Multiple sclerosis","Erythema nodosum","SLE","Ulcerative colitis","Autoimmune hepatitis",null,null,null,null,null,null,null,null,null,null],"parameter_in_program":["immune_diseases_in_the_study","immune_diseases_in_the_study","immune_diseases_in_the_study","immune_diseases_in_the_study","immune_diseases_in_the_study","immune_diseases_in_the_study","immune_diseases_in_the_study","immune_diseases_in_the_study","immune_diseases_in_the_study","immune_diseases_in_the_study","groups_of_prompts","groups_of_prompts","groups_of_prompts",null,null,null,null,null,null,null],"set_in_step":[null,null,null,null,null,null,null,null,null,null,"07_algorithms","07_algorithms","07_algorithms",null,null,null,null,null,null,null],"notes":[null,null,null,null,null,null,null,null,null,null,"for some data sources, some groups may be not caculated; for TEST only PC and HOSP_DISP are calculates","for some data sources, some groups may be not caculated; for TEST only PC and HOSP_DISP are calculates","for some data sources, some groups may be not caculated; for TEST only PC and HOSP_DISP are calculates",null,null,null,null,null,null,null]},"columns":[{"id":"parameter","name":"parameter","type":"character"},{"id":"value","name":"value","type":"character"},{"id":"label","name":"label","type":"character"},{"id":"parameter_in_program","name":"parameter_in_program","type":"character"},{"id":"set_in_step","name":"set_in_step","type":"character"},{"id":"notes","name":"notes","type":"character"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"bed6750dd64de800f8223ce30636f872"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Example" class="tabcontent">
<div id="htmlwidget-4" class="reactable html-widget" style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-4">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"D3_cohort_E_GRAVES_AESI":["person_id","P01","P02","P03","P05","P06",null,"E_GRAVES_AESI_narrow","person_id","P01","P01","P02","P03","P03","P04","P05","P05","P06","P06",null],"...2":["study_entry_date","1","1","1","1","1",null,null,"date","100","150","1000","-100","100","50","100","120","-7000","100",null],"...3":["study_exit_date","2190","1080","2190","2190","2190",null,null,"end_date_record",null,null,null,null,null,null,null,null,null,null,null],"...4":["has_a_code_in_the_study_period_E_GRAVES_AESI","1","1","1","1","1",null,null,"codvar",null,null,null,null,null,null,null,null,null,null,null],"...5":["exclude_because_exist_code_during_lookback_E_GRAVES_AESI","0","0","1","0","1",null,null,"event_record_vocabulary",null,null,null,null,null,null,null,null,null,null,null],"...6":["exclude_because_exist_exclusion_criterion_during_lookback_E_GRAVES_AESI","0","0","0","0","0",null,null,"text_linked_to_event_code",null,null,null,null,null,null,null,null,null,null,null],"...7":["enter_cohort_E_GRAVES_AESI","1","1","0","1","0",null,null,"event_free_text",null,null,null,null,null,null,null,null,null,null,null],"...8":["cohort_entry_date_E_GRAVES_AESI","100","1000",null,"100",null,null,null,"present_on_admission",null,null,null,null,null,null,null,null,null,null,null],"...9":["cause_for_not_entering_followup_E_GRAVES_AESI","0","2",null,"0",null,null,null,"laterality_of_event",null,null,null,null,null,null,null,null,null,null,null],"...10":["entering_follow_up_postponed_E_GRAVES_AESI","0","0",null,"1",null,null,null,"meaning_renamed","hospitalisation_primary","primary_care_diagnosis","hospitalisation_primary","primary_care_diagnosis","hospitalisation_primary","primary_care_diagnosis","hospitalisation_primary","primary_care_diagnosis","primary_care_diagnosis","hospitalisation_primary",null],"...11":["entering_follow_up_E_GRAVES_AESI","1","0",null,"1",null,null,null,"origin_of_event",null,null,null,null,null,null,null,null,null,null,null],"...12":["start_follow_up_E_GRAVES_AESI","190",null,null,"280",null,null,null,"visit_occurrence_id",null,null,null,null,null,null,null,null,null,null,null],"...13":["has_a_code_in_the_study_period_E_GRAVES_AESI_PC","1","0","0","1","0",null,null,"Col",null,null,null,null,null,null,null,null,null,null,null],"...14":["exclude_because_exist_code_during_lookback_E_GRAVES_AESI_PC","0",null,null,"0",null,null,null,"Table_cdm",null,null,null,null,null,null,null,null,null,null,null],"...15":["exclude_because_exist_exclusion_criterion_during_lookback_E_GRAVES_AESI_PC","0",null,null,"0",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...16":["enter_cohort_E_GRAVES_AESI_PC","1","0","0","1","0",null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...17":["cohort_entry_date_E_GRAVES_AESI_PC","150",null,null,"120",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...18":["cause_for_not_entering_followup_E_GRAVES_AESI_PC","0",null,null,"0",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...19":["entering_follow_up_postponed_E_GRAVES_AESI_PC","0",null,null,"1",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...20":["entering_follow_up_E_GRAVES_AESI_PC","1",null,null,"1",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...21":["start_follow_up_E_GRAVES_AESI_PC","240",null,null,"300",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...22":["has_a_code_in_the_study_period_E_GRAVES_AESI_HOSP_DISP","1","1","1","1","1",null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...23":["exclude_because_exist_code_during_lookback_E_GRAVES_AESI_HOSP_DISP","0","0","0","0","0",null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...24":["exclude_because_exist_exclusion_criterion_during_lookback_E_GRAVES_AESI_HOSP_DISP","0","0","0","0","0",null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...25":["enter_cohort_E_GRAVES_AESI_HOSP_DISP","1","1","1","1","1",null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...26":["cohort_entry_date_E_GRAVES_AESI_HOSP_DISP","100","1000","100","100","100",null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...27":["cause_for_not_entering_followup_E_GRAVES_AESI_HOSP_DISP","0","1080","0","0","0",null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...28":["entering_follow_up_postponed_E_GRAVES_AESI_HOSP_DISP","0","2","0","1","0",null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...29":["entering_follow_up_E_GRAVES_AESI_HOSP_DISP","1","0","1","1","1",null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...30":["start_follow_up_E_GRAVES_AESI_HOSP_DISP","190","0","190","280","190",null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...31":["has_a_code_in_the_study_period_E_GRAVES_AESI_HOSP_SPEC_DISP",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...32":["exclude_because_exist_code_during_lookback_E_GRAVES_AESI_HOSP_SPEC_DISP",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...33":["exclude_because_exist_exclusion_criterion_during_lookback_E_GRAVES_AESI_HOSP_SPEC_DISP",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...34":["enter_cohort_E_GRAVES_AESI_HOSP_SPEC_DISP",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...35":["cohort_entry_date_E_GRAVES_AESI_HOSP_SPEC_DISP",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...36":["cause_for_not_entering_followup_E_GRAVES_AESI_HOSP_SPEC_DISP",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...37":["entering_follow_up_postponed_E_GRAVES_AESI_HOSP_SPEC_DISP",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...38":["entering_follow_up_E_GRAVES_AESI_HOSP_SPEC_DISP",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...39":["start_follow_up_E_GRAVES_AESI_HOSP_SPEC_DISP",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"D3_cohort_E_GRAVES_AESI","name":"D3_cohort_E_GRAVES_AESI","type":"character"},{"id":"...2","name":"...2","type":"character"},{"id":"...3","name":"...3","type":"character"},{"id":"...4","name":"...4","type":"character"},{"id":"...5","name":"...5","type":"character"},{"id":"...6","name":"...6","type":"character"},{"id":"...7","name":"...7","type":"character"},{"id":"...8","name":"...8","type":"character"},{"id":"...9","name":"...9","type":"character"},{"id":"...10","name":"...10","type":"character"},{"id":"...11","name":"...11","type":"character"},{"id":"...12","name":"...12","type":"character"},{"id":"...13","name":"...13","type":"character"},{"id":"...14","name":"...14","type":"character"},{"id":"...15","name":"...15","type":"character"},{"id":"...16","name":"...16","type":"character"},{"id":"...17","name":"...17","type":"character"},{"id":"...18","name":"...18","type":"character"},{"id":"...19","name":"...19","type":"character"},{"id":"...20","name":"...20","type":"character"},{"id":"...21","name":"...21","type":"character"},{"id":"...22","name":"...22","type":"character"},{"id":"...23","name":"...23","type":"character"},{"id":"...24","name":"...24","type":"character"},{"id":"...25","name":"...25","type":"character"},{"id":"...26","name":"...26","type":"character"},{"id":"...27","name":"...27","type":"character"},{"id":"...28","name":"...28","type":"character"},{"id":"...29","name":"...29","type":"character"},{"id":"...30","name":"...30","type":"character"},{"id":"...31","name":"...31","type":"character"},{"id":"...32","name":"...32","type":"character"},{"id":"...33","name":"...33","type":"character"},{"id":"...34","name":"...34","type":"character"},{"id":"...35","name":"...35","type":"character"},{"id":"...36","name":"...36","type":"character"},{"id":"...37","name":"...37","type":"character"},{"id":"...38","name":"...38","type":"character"},{"id":"...39","name":"...39","type":"character"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"94613421948da1872fc86c214c203551"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>