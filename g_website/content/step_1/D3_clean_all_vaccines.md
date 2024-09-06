---
weight: 10
name_excel: "D3_clean_all_vaccines.xlsx"
description: "This dataset contains the records of all the curated doses of all vaccines in the instance listed in Table 4 of the SAP, including the curated covid vaccines. It is obtained from the original conceptsets datastes by replicating each vaccination record as many times as the indicators that it is used for, see the first example in the tab Example: a record with Vacco Id DIP-HIB-PER-POL-TET is replicated 3 times, once per the indicator DPT, once per the indicator HiB, and once for the indicator Pol. Then, each record is labelled with various exclusion citeria, most importantly, records with dats 30 daya apart form a previous record as marked as 'duplicates'. In the next step, all the record labelled as 'removed row' will be removed"
slug: "D3_clean_all_vaccines"
datetime: 1.7253066e+09
title: D3_clean_all_vaccines
author: ''
date: '2024-09-02'
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
<script type="application/json" data-for="htmlwidget-1">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"medatata_name":["Name of the dataset","Content of the dataset","Unit of observation","Dataset where the list of UoOs is fully listed and with 1 record per UoO","How many observations per UoO","NxUoO","Variables capturing the UoO","Primary key","Parameters",null,null,null,null,null,null,null,null,null,null,null],"metadata_content":["D3_clean_all_vaccines","This dataset contains the records of all the curated doses of all vaccines in the instance listed in Table 4 of the SAP, including the curated covid vaccines. It is obtained from the original conceptsets datastes by replicating each vaccination record as many times as the indicators that it is used for, see the first example in the tab Example: a record with Vacco Id DIP-HIB-PER-POL-TET is replicated 3 times, once per the indicator DPT, once per the indicator HiB, and once for the indicator Pol. Then, each record is labelled with various exclusion citeria, most importantly, records with dats 30 daya apart form a previous record as marked as 'duplicates'. In the next step, all the record labelled as 'removed row' will be removed","root indicators for all persons in the instance",null,"as many as the doses for that root_indicator, including duplicates",">= 0","person_id root_indicator",null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"medatata_name","name":"medatata_name","type":"character"},{"id":"metadata_content","name":"metadata_content","type":"character"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"a00b6704e1f089764e2acf9b5da06fa1"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Data Model" class="tabcontent">
<div id="htmlwidget-2" class="reactable html-widget" style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-2">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"VarName":["person_id","date_curated","dose_curated","manufacturer_curated","vacco_id","concept_vacco_id","root_indicator","imputed_dose","duplicated_records","missing_date","distance_too_short","duplicated_records_final","removed_row",null,null,null,null,null,null,null],"Description":["unique person identifier",null,null,null,"label from the VaccO ontology","for each vacco_id there may be multiple ways of retrieving the records: from ATC (either from VACCINES or MEDICINES) or from vx_type (from VACCINES). Each of those originate a concept. The methods are specified in SAFETY-VAC_vaccines_28Mar24 and are assigned in 03_concepts","root of the indicator, as specified in SAFETY-VAC_indicators_28Mar24, i.e., Table 7 of the Protocol. The same indicator may be populated by multiple VaccoIDs, e.g., the indicator Pol2 may be obtained by a first dose of POL and a second dose of DIP-PER-POL-TET. The two records are copied with root_indicator Pol and doses curated are assigned on the base of the root_indicator","flag that the curated dose is imputed","to be excluded because it's a duplicated record before all the transformations","to be excluded because date is missing","to be excluded because distance from another record of the same root_indicator is distant less than 30 days","to be excluded because after all tranformation it is a duplicated record","1 if any exclusion criterion is 1",null,null,null,null,null,null,null],"Format":["character","date","integer","character","character","character",null,"binary","binary","binary","binary","binary","binary",null,null,null,null,null,null,null],"Vocabulary":[null,null,"1, 2 , 3,4",null,"VaccO ontology, as specified in SAFETY-VAC_vaccines_28Mar24","VaccO ontlogy + one among ATC, MED, VXTYPE","\"MCV\" = measles-containing vaccine\r\n\"DTP\" = diphteria-tetatnus-pertussis\r\n\"Hib\" = Haemophilus influenzae type B\r\n\"HepB\" = Hepatatis B\r\n\"Pol\" = Polio\r\n\"PCV\" = Pneumococcal conjugate vaccines\r\n\"Varicella\" = Varicella\r\n\"BCG\" = Bacille Calmette-Guérin vaccine\r\n\"HPV\" = Human papillomavirus vaccine\r\n\"RotaC\" = Rotavirus\r\n\"Meningococcal\" = Meningococcal vaccine\r\n\"Influenza\" =Infuenza\r\n\"Coronavirus\" = Coronavirus",null,"1 = to be excluded\r\n0 = otherwise","1 = to be excluded\r\n0 = otherwise","1 = to be excluded\r\n0 = otherwise","1 = to be excluded\r\n0 = otherwise","1 = to be excluded\r\n0 = otherwise",null,null,null,null,null,null,null],"Parameters":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Notes and examples":["from CDM PERSONS",null,"doses higher than the vaccine-specific maximum dosage are excluded",null,"note that the symbol '-' of the original VaccO ontology has been replaced by the symbol '_' because the former cannot be part of a file name in , so examples are\r\n\r\n\"DIP_HEB_PER_POL_TET\"     \r\n\"DIP_HIB_PER_TET\"         \r\n\"DIP_TET\"                 \r\n\"HEB\"                     \r\n\"POL\"                     \r\n\"MEA_MUM_RUB\"             \r\n\"MUM\"                     \r\n\"RVV\"                     \r\n\"COV\"                     \r\n\"HEZ\"                     \r\n\"HIB_POL\"",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Source tables and variables":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Retrieved":["yes","yes","yes","yes","yes",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Calculated":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Algorithm_id":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Rule":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"VarName","name":"VarName","type":"character"},{"id":"Description","name":"Description","type":"character"},{"id":"Format","name":"Format","type":"character"},{"id":"Vocabulary","name":"Vocabulary","type":"character"},{"id":"Parameters","name":"Parameters","type":"logical"},{"id":"Notes and examples","name":"Notes and examples","type":"character"},{"id":"Source tables and variables","name":"Source tables and variables","type":"logical"},{"id":"Retrieved","name":"Retrieved","type":"character"},{"id":"Calculated","name":"Calculated","type":"logical"},{"id":"Algorithm_id","name":"Algorithm_id","type":"logical"},{"id":"Rule","name":"Rule","type":"logical"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"58bac99c02a0f3966888c0a8538c0652"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Parameters" class="tabcontent">
<div id="htmlwidget-3" class="reactable html-widget" style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-3">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"parameter in the variable name":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"values":["TUB","DIP_PER_TET","DIP_HEB_PER_POL_TET","DIP_HIB_PER_TET","DIP_TET","HEB","POL","MEA_MUM_RUB","MUM","RVV","COV","HEZ","HIB_POL","PER","DIP_HEB_PER_TET","DIP_HIB_PER_POL_TET","DIP","INF","HIB","MEA"],"name of macro":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"parameter in the variable name","name":"parameter in the variable name","type":"logical"},{"id":"values","name":"values","type":"character"},{"id":"name of macro","name":"name of macro","type":"logical"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"7e3d8a09a054ca7e28b0e348bfa2ed60"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Example" class="tabcontent">
<div id="htmlwidget-4" class="reactable html-widget" style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-4">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"person_id":["P001","P001","P001","P001","P001","P001","P002","P002","P002","P003","P003","P003","P003","P004","P004",null,null,null,null,null],"date":["2021-05-02T00:00:00Z","2021-07-27T00:00:00Z","2021-05-02T00:00:00Z","2021-07-27T00:00:00Z","2021-05-02T00:00:00Z","2021-07-27T00:00:00Z","2021-08-20T00:00:00Z","2021-07-30T00:00:00Z","2022-07-30T00:00:00Z","2021-01-01T00:00:00Z","2021-01-25T00:00:00Z","2021-04-01T00:00:00Z","2021-10-12T00:00:00Z","2021-05-02T00:00:00Z","2021-07-27T00:00:00Z",null,null,null,null,null],"vx_record_date":["2021-05-02T00:00:00Z","2021-07-27T00:00:00Z","2021-05-02T00:00:00Z","2021-07-27T00:00:00Z","2021-05-02T00:00:00Z","2021-07-27T00:00:00Z","2021-08-20T00:00:00Z","2021-07-30T00:00:00Z","2022-07-30T00:00:00Z","2021-01-01T00:00:00Z","2021-01-25T00:00:00Z","2021-04-01T00:00:00Z","2021-10-12T00:00:00Z","2021-05-02T00:00:00Z","2021-05-02T00:00:00Z",null,null,null,null,null],"vx_dose":[1,2,1,2,1,2,1,2,1,1,2,3,4,1,1,"NA","NA","NA","NA","NA"],"vx_manufacturer":[null,null,null,null,null,null,"pfizer","pfizer",null,"astrazeneca","astrazeneca","pfizer","moderna",null,null,null,null,null,null,null],"date_curated":["2021-05-02T00:00:00Z","2021-07-27T00:00:00Z","2021-05-02T00:00:00Z","2021-07-27T00:00:00Z","2021-05-02T00:00:00Z","2021-07-27T00:00:00Z","2021-08-20T00:00:00Z","2021-07-30T00:00:00Z","2022-07-30T00:00:00Z","2021-01-01T00:00:00Z","2021-01-25T00:00:00Z","2021-04-01T00:00:00Z","2021-10-12T00:00:00Z","2021-05-02T00:00:00Z","2021-07-27T00:00:00Z",null,null,null,null,null],"dose_curated":[1,2,1,2,1,2,1,2,1,1,2,3,4,1,1,"NA","NA","NA","NA","NA"],"manufacturer_curated":[null,null,null,null,null,null,"pfizer","pfizer",null,"astrazeneca","astrazeneca","pfizer","moderna",null,null,null,null,null,null,null],"vacco_id":["DIP-HIB-PER-POL-TET","DIP-HIB-PER-POL-TET","DIP-HIB-PER-POL-TET","DIP-HIB-PER-POL-TET","DIP-HIB-PER-POL-TET","DIP-HIB-PER-POL-TET","COV","COV","HPV","COV","COV","COV","COV","DIP-PER-TET","DIP-PER-TET",null,null,null,null,null],"concept_vacco_id":["DIP-HIB-PER-POL-TETATC","DIP-HIB-PER-POL-TETVXTYPE","DIP-HIB-PER-POL-TETATC","DIP-HIB-PER-POL-TETVXTYPE","DIP-HIB-PER-POL-TETATC","DIP-HIB-PER-POL-TETVXTYPE","COVATC","COVATC","HPVATC","COVATC","COVVXTYPE","COVATC","COVATC","DIP-PER-TETATC","DIP-PER-TETVXTYPE",null,null,null,null,null],"root_indicator":["DPT","DPT","Pol","Pol",null,null,"Coronavirus","Coronavirus","HPV ","Coronavirus","Coronavirus","Coronavirus","Coronavirus","DPT","DPT",null,null,null,null,null],"duplicated_records":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"NA","NA","NA","NA","NA"],"missing_date":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"NA","NA","NA","NA","NA"],"distance_too_short":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"NA","NA","NA","NA","NA"],"duplicated_records_final":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,"NA","NA","NA","NA","NA"],"removed_row":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,"NA","NA","NA","NA","NA"],"imputed_dose":["FALSE","FALSE","TRUE","TRUE","FALSE","FALSE","FALSE","FALSE","FALSE","FALSE","FALSE","FALSE","FALSE","FALSE","FALSE",null,null,null,null,null]},"columns":[{"id":"person_id","name":"person_id","type":"character"},{"id":"date","name":"date","type":"Date"},{"id":"vx_record_date","name":"vx_record_date","type":"Date"},{"id":"vx_dose","name":"vx_dose","type":"numeric"},{"id":"vx_manufacturer","name":"vx_manufacturer","type":"character"},{"id":"date_curated","name":"date_curated","type":"Date"},{"id":"dose_curated","name":"dose_curated","type":"numeric"},{"id":"manufacturer_curated","name":"manufacturer_curated","type":"character"},{"id":"vacco_id","name":"vacco_id","type":"character"},{"id":"concept_vacco_id","name":"concept_vacco_id","type":"character"},{"id":"root_indicator","name":"root_indicator","type":"character"},{"id":"duplicated_records","name":"duplicated_records","type":"numeric"},{"id":"missing_date","name":"missing_date","type":"numeric"},{"id":"distance_too_short","name":"distance_too_short","type":"numeric"},{"id":"duplicated_records_final","name":"duplicated_records_final","type":"numeric"},{"id":"removed_row","name":"removed_row","type":"numeric"},{"id":"imputed_dose","name":"imputed_dose","type":"character"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"9c277f92159de38d79d3566edc3f17e8"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>