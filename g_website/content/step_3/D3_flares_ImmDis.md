---
weight: 3
name_excel: "D3_flares_ImmDis.xlsx"
description: ""
slug: "D3_flares_ImmDis"
datetime: 1.7268637e+09
title: D3_flares_ImmDis
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
<script type="application/json" data-for="htmlwidget-1">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"metadata_name":["Name of the dataset","Content of the dataset","Unit of observation","Dataset where the list of UoOs is fully listed and with 1 record per UoO","How many observations per UoO","NxUoO","Variables capturing the UoO","Primary key","Parameters",null,null,null,null,null,null,null,null,null,null,null],"metadata_content":["D3_flares_{ImmDis}","Persons that enter the follow up in the cohort of {ImmDis}, with all their flares","persons that are in the cohort of {ImmDis} with entering_follow_up_{ImmDis} == 1","D3_cohort_{ImmDis} where entering_follow_up_{ImmDis} == 1","as many as the flares the person is experiencing during follow up",">= 1","person_id","person_id date_flare_{ImmDis}","ImmDis",null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"metadata_name","name":"metadata_name","type":"character"},{"id":"metadata_content","name":"metadata_content","type":"character"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"0563a82813e8ae1779952979fde314c5"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Data Model" class="tabcontent">
<div id="htmlwidget-2" class="reactable html-widget" style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-2">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"Varname":["person_id","date_flare_{ImmDis}",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Description":[null,"date of a flare of {ImmDis}",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Format":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Vocabulary":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Description / Notes":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Parameters":[null,"ImmDis",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Source tables and variables":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Retrieved":["yes",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Calculated":[null,"yes",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Algorithm_id":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"Rule":[null,"retrieve all distinct person_id date_flare_{ImmDis} from D3_components_flare_TD_{ImmDis}",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"Varname","name":"Varname","type":"character"},{"id":"Description","name":"Description","type":"character"},{"id":"Format","name":"Format","type":"logical"},{"id":"Vocabulary","name":"Vocabulary","type":"logical"},{"id":"Description / Notes","name":"Description / Notes","type":"logical"},{"id":"Parameters","name":"Parameters","type":"character"},{"id":"Source tables and variables","name":"Source tables and variables","type":"logical"},{"id":"Retrieved","name":"Retrieved","type":"character"},{"id":"Calculated","name":"Calculated","type":"character"},{"id":"Algorithm_id","name":"Algorithm_id","type":"logical"},{"id":"Rule","name":"Rule","type":"character"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"e96f10388cec3b774b275d18c394d289"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Parameters" class="tabcontent">
<div id="htmlwidget-3" class="reactable html-widget" style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-3">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"parameter":["ImmDis","ImmDis","ImmDis","ImmDis","ImmDis","ImmDis","ImmDis","ImmDis","ImmDis","ImmDis",null,null,null,null,null,null,null,null,null,null],"value":["E_GRAVES_AESI","Im_HASHIMOTO_AESI","V_PAN_AESI","M_ARTRHEU_AESI","M_ARTPSORIATIC_AESI","N_DEMYELMS_AESI","SK_ERYTHEMANODOSUM_AESI","Im_SLE_AESI","D_ULCERATIVECOLITIS_AESI","D_HEPATITISAUTOIMMUNE_AESI",null,null,null,null,null,null,null,null,null,null],"label":["Graves","Hashimoto","Polyarteritis nodose","Rheumatoid arthritis","Psoriatic arthritis","Multiple sclerosis","Erythema nodosum","SLE","Ulcerative colitis","Autoimmune hepatitis",null,null,null,null,null,null,null,null,null,null],"parameter_in_program":["immune_diseases_in_the_study","immune_diseases_in_the_study","immune_diseases_in_the_study","immune_diseases_in_the_study","immune_diseases_in_the_study","immune_diseases_in_the_study","immune_diseases_in_the_study","immune_diseases_in_the_study","immune_diseases_in_the_study","immune_diseases_in_the_study",null,null,null,null,null,null,null,null,null,null],"set_in_step":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"notes":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"parameter","name":"parameter","type":"character"},{"id":"value","name":"value","type":"character"},{"id":"label","name":"label","type":"character"},{"id":"parameter_in_program","name":"parameter_in_program","type":"character"},{"id":"set_in_step","name":"set_in_step","type":"logical"},{"id":"notes","name":"notes","type":"logical"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"43e20ea6d74466c41071f549c1995617"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
<div id="Example" class="tabcontent">
<div id="htmlwidget-4" class="reactable html-widget" style="width:auto;height:600px;"></div>
<script type="application/json" data-for="htmlwidget-4">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"D3_flares_{ImmDis}":["person_id","P1",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"...2":["date_flare_{ImmDis}","300",null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null]},"columns":[{"id":"D3_flares_{ImmDis}","name":"D3_flares_{ImmDis}","type":"character"},{"id":"...2","name":"...2","type":"character"}],"sortable":false,"searchable":true,"pagination":false,"highlight":true,"bordered":true,"striped":true,"style":{"maxWidth":1800},"height":"600px","dataKey":"c6fb71e28d9824b3779a3775c8974ff5"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>
</div>
