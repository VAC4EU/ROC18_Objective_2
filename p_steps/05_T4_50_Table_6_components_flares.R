############################################################
#                                                          #
####   CREATE D5_Table_6_Impact_of_component_flare_{ImmDis} ####
# 
#                                                           #
############################################################

# author: Rosa Gini
# 
# v 1.1.0 2 Oct 2024
#
# handled case when components dataset is empty
#
# v 1.0.0  28 Sep 2024

#########################################

#########################################
# assign input and output directories

if (TEST){ 
  immdis <- "E_GRAVES_AESI"
  testname <- "test_05_T4_50_Table_6_components_flares"
  thisdirinput <- file.path(dirtest,testname)
  thisdirconceptsets <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output_program")
  thisdirexp <- file.path(dirtest,testname,"g_output_program")
  thisdirfigure <- paste0(file.path(dirtest,testname,"g_output_program"),"/")
  dir.create(thisdiroutput, showWarnings = F)
  immune_diseases_in_this_step <- c(immdis)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirexp <- dirtablesubpop[[subpop]]
  thisdirconceptsets <- dirconceptsets
  thisdirfigure <- paste0(direxp,"/figures/")
  dir.create(thisdirfigure, showWarnings = F)
  immune_diseases_in_this_step <- immune_diseases_in_the_study
}

# load input datasets to be used by all diseases: none

for (immdis in immune_diseases_in_this_step){ 
  
  print(paste0("Analysis of components for flares of ", immdis))
  
  component_dataset <- readRDS(file.path(thisdirinput,paste0("D4_component_dataset_",immdis,".rds")))
  component_dataset[, ds := thisdatasource]
  
  name_intermediate <- paste0(thisdiroutput,"/composites_",immdis)
  thisnameoutput <- paste0(thisdiroutput,"/data_Table_6_components_flares_raw_",immdis)
  figure_name <- paste0("Figure_components_flares_",thisdatasource,"_",immdis)
  

  if (nrow(component_dataset > 0)) {
    output <- ApplyComponentStrategy(dataset = component_dataset,
           individual = F, ## F -> data counts
           intermediate_output = T,
           intermediate_output_name = name_intermediate,
           components = c("is_flare_Diagnoses", # 1
                          "is_flare_Medications", # 2
                          "is_flare_Procedures", # 3
                          "is_flare_Emergency", # 4
                          "is_flare_Hosp", # 5
                          "is_flare_Flare" # 6
                          ),
                          labels_of_components = c("Diagnoses",  #1
                                                   "Medicines",  #2
                                                   "Procedures", #3
                                                   "Emergency",  #4
                                                   "Hospitalization", #5
                                                   "Flare" #6
                          ),   
                          composites_to_be_created = list(
                            list(1, 6),          #7
                            list(4, 5),          #8
                            list(1, 6, 4, 5),    #9
                            list(2, 3)           #10
                          ),
                          labels_of_composites_to_be_created = c(
                            "Dia + flares (1 or 6)",         #7
                            "ER + H (4 or 5)",#8
                            "Dia + flares + ER + H (7 or 8)",    #9
                            "Med + proc (2 or 3)"   #10
                          ),
                          pairs_to_be_compared=list(
                            list(1,6), #11
                            list(4,5), #12
                            list(7,2), #13
                            list(7,3), #14
                            list(8,2), #15
                            list(8,3), #16
                            list(7,8), #17
                            list(9,2), #18
                            list(9,3), #19
                            list(9,10) #20
                          ),
                labels_of_pairs_to_be_compared = c(
                  "Diagnosis vs flares (1 vs 6)", #11
                  "ER vs H (4 vs 5)", #12 
                  "(Dia + flares) vs medicines (7 vs 2)", #13
                  "(Dia + flares) vs procedures (7 vs 3)", #14
                  "(ER + H) vs medicines (8 vs 2)", #15
                  "(ER + H) vs procedures (8 vs 3)", #16
                  "(Dia + flares) vs (ER + H) (7 vs 8)", #17
                  "(Dia + flares + ER + H) vs medicines (9 vs 2)", #18
                  "(Dia + flares + ER + H) vs procedures (9 vs 3)", #19
                  "(Dia + flares + ER + H) vs (med + proc)  (9 vs 10)" #20
                          ),
                          figure_name = figure_name,
                          K=1000,
                          # strata = list("ds"),
                          count_var = "N",
                          figure = T,
                          aggregate = F ,
                          output_name = thisnameoutput,
                          dirfigure = thisdirfigure
           )
                                   
  load(paste0(thisnameoutput,".RData"))
  processing <- get(paste0("data_Table_6_components_flares_raw_",immdis))
  processing <- processing[,.(ord_alg, N_, PROP_, PROP_10, PROP_11, PROP_01)]
  processing[, N := as.character(N_)]
  processing <- processing[ N_ > 0 & N_ < 5, N := "< 5"]
  setnames(processing, "ord_alg", "Algorithm")
  for (var in c("PROP_", "PROP_10", "PROP_11", "PROP_01")) {
    processing[, (var) := round(get(var), 1)]
  }
  for (var in c("PROP_10", "PROP_11", "PROP_01")) {
    processing[1:10, (var) := NA]
  }

  processing[,PROP_L:=  PROP_10 + PROP_11]
  processing[,PROP_R:=  PROP_01 + PROP_11]
  
  processing <- processing[,.(Algorithm, N, PROP_, PROP_L, PROP_R, PROP_10, PROP_11, PROP_01)]
  }else{
    processing <- data.table()
    processing[, Algorithm := ""]
    for (var in c("N", "PROP_", "PROP_L", "PROP_R", "PROP_10", "PROP_11", "PROP_01")) {
      processing[, (var) := NA_integer_ ]
    }
  }
  setnames(processing, c("PROP_", "PROP_L", "PROP_R", "PROP_10", "PROP_11", "PROP_01"), c("Total in the algorithm (left-hand OR right-hand)", "Left-hand","Right-hand", "Unique contribution of the left-hand component (left-hand AND NOT right-hand)", "Overlap of both components (left-hand AND right-hand)", "Unique contribution of the right-hand component (right-hand AND NOT left-hand)"))
  
  
  
  ########################################
  # save

  outputfile <- processing
  nameoutput <- paste0("D5_Table_6_Impact_of_component_flare_",thisdatasource,"_",immdis)
  assign(nameoutput, outputfile)
  saveRDS(outputfile, file = file.path(thisdirexp, paste0(nameoutput,".rds")))
  fwrite(outputfile, file = file.path(thisdirexp, paste0(nameoutput,".csv")))

}


