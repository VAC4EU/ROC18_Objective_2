############################################################
#             TEMPLATE                                             #


############################################################
#                                                          #
####   CREATE D5_KM ####
# 
#                                                          #
############################################################

# 
# v 1.1.0  2 Oct Rosa Gini: added option 'extend' to address case of missing data 
# 
# author: Belen Castillo Cano
# 
# v 1.0.0  XXX

#########################################

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_T4_60_KM"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname)
  thisdirexp <-   direxp
  dir.create(thisdiroutput, showWarnings = F)
  immune_diseases_in_this_step <- immune_diseases_in_the_study <- c("E_GRAVES_AESI")
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtablesubpop[[subpop]]
  thisdirexp <- direxp
  immune_diseases_in_this_step <- immune_diseases_in_the_study
}

for (immdis in immune_diseases_in_this_step){
  print(paste0("Survival analysis for ",immdis))
  
  #########################################
  # load input dataset
  
  input_dataset <- readRDS(file.path(thisdirinput, paste0("D4_analytical_dataset_KM_",immdis,".rds") ))
  
  #########################################
  # process input datasets to obtain output
  
  # clean variables
  
  for (varname in c("age_at_cohort_entry_date","days","flare")){
    setnames(input_dataset,paste0(varname,"_",immdis),varname)
  }
  
  if (nrow(input_dataset) > 0) {
    
    
    # BELEN from here
    
    # SETTINGS & LOAD PACKAGES
    
    # Check if writexl is installed, if not, install it
    pkgs <- installed.packages()[, 1]
    if(!"writexl" %in% pkgs) {
      install.packages(writexl)
    }
    library(survival)
    library(writexl)
  
    #Fit a Kaplan-Meier:
    kmfit1 <- survfit(Surv(days, flare)~1, data=input_dataset)
  
    
    #n: total number of observations in each curve 
    #time: the time points at which the curve has a step
    #n.risk: the number of subjects at risk at t
    #n.event: the number of events that occur at time t
    #n.censor: for counting process data only, the number of subjects who exit the risk set, without an event, at time t. (For right censored data, this number can be computed from the successive values of the number at risk)
    #surv: the estimate of survival at the time t+0
    #std.err: for a survival curve this contains standard error of the cumulative hazard or -log(survival), for a multi-state curve it contains the standard error of prev. This difference is a reflection of the fact that each is the natural calculation for that case
    #cumhaz: optional. Contains the cumulative hazard for each possible transition
    #lower: options lower confidence limit for the survival curve or pstate
    #upper: optional upper confidence limit for the survival curve or pstate
    #conf.int: the level of the confidence limits, e.g. 90 or 95%.
   
    #Export in ".xlsx" file the survival probability until day 365:
    
    #Impute the survival probabilities and confidence intervals by assigning the previous values.
    impute_all_points <- function(kmfit, day0 = 1, dayf = 365){
      
      final_df <- data.frame(day = day0:dayf)
      #Create a data frame with the survival times, probability and confidence interval:
      survival_df <- data.frame(
        day = kmfit$time,
        survival = round(kmfit$surv, 3),
        ll = round(kmfit$lower, 3),
        ul = round(kmfit$upper, 3)
      )
      survival_data_365 <- survival_df[survival_df$day<=dayf, ]
      
      final_df <- merge(final_df, survival_data_365, by = "day", all = TRUE)
      
      # Check day 1 if it's finite or not
      if(is.na(final_df$survival[1])){
        final_df$survival[1] <- 1 # Assign 1 for unregistered data.
      }
      
      for(i in 2:nrow(final_df)){
        # If no observation was recorded, assign the last value.
        if(is.na(final_df$survival[i])){
          final_df$survival[i] <- final_df$survival[i - 1]
          final_df$ll[i] <- final_df$ll[i - 1]
          final_df$ul[i] <- final_df$ul[i - 1]
        }
      }
      
      final_df
    }
    
    survival_data_365 <- impute_all_points(kmfit1)
    
    #Export to excel file:
    # write_xlsx(survival_data_365, "D5_data_km_figure.xlsx")
    
    
    print(paste0("Cumulative incidence of ",immdis))
    
    #Cumulative incidence:
    #Create variable "ageband" (0=<18y, 18=18-59y, 60=60y+)
    ageband <- cut(input_dataset$age_at_cohort_entry_date,
                    breaks = c(-Inf, 17, 59, Inf),   
                    labels = c(0, 18, 60),           
                    right = TRUE)    
  
    
    input_dataset$age_cat<-ageband
    
    #Unstratified cumulative incidence:
      #Survival at 180 and 365 days:
      summary_kmfit1 <- summary(kmfit1, times=c(180,365), extend = T)
      #Incidence and CI:
      incidence_180 <- 1 - summary_kmfit1$surv[1]
      lower_180 <- 1 - summary_kmfit1$upper[1]
      upper_180 <- 1 - summary_kmfit1$lower[1]
      
      incidence_365 <- 1 - summary_kmfit1$surv[2]
      lower_365 <- 1 - summary_kmfit1$upper[2]
      upper_365 <- 1 - summary_kmfit1$lower[2]
      
    #Stratified cumulative incidence by sex:
      #Convert the variable 'sex' to a factor:
      input_dataset$sex_at_instance_creation <- factor(input_dataset$sex_at_instance_creation, levels=c("F","M","O"))
      #Fit a Kaplan-Meier stratified by sex:
      kmfitsex <- survfit(Surv(days, flare)~sex_at_instance_creation, data=input_dataset)
      #Survival at 180 and 365 days:
      summary_kmfitsex <- summary(kmfitsex, times=c(180,365), extend = T)
      #Incidence and CI by sex:
        #Female:
        incidence_180_F <- 1 - summary_kmfitsex$surv[1]
        lower_180_F <- 1 - summary_kmfitsex$upper[1]
        upper_180_F <- 1 - summary_kmfitsex$lower[1]
        
        incidence_365_F <- 1 - summary_kmfitsex$surv[2]
        lower_365_F <- 1 - summary_kmfitsex$upper[2]
        upper_365_F <- 1 - summary_kmfitsex$lower[2]
        
        #Male:
        incidence_180_M <- 1 - summary_kmfitsex$surv[3]
        lower_180_M <- 1 - summary_kmfitsex$upper[3]
        upper_180_M <- 1 - summary_kmfitsex$lower[3]
        
        incidence_365_M <- 1 - summary_kmfitsex$surv[4]
        lower_365_M <- 1 - summary_kmfitsex$upper[4]
        upper_365_M <- 1 - summary_kmfitsex$lower[4]
        
    #Stratified cumulative incidence by ageband: 
      #Fit a Kaplan-Meier stratified by ageband:
      kmfitage <- survfit(Surv(days, flare)~age_cat, data=input_dataset)
      #Survival at 180 and 365 days:
      summary_kmfitage <- summary(kmfitage, times=c(180,365), extend = T)
      #Incidence and CI by ageband:
        #0:
        incidence_180_0 <- 1 - summary_kmfitage$surv[1]
        lower_180_0 <- 1 - summary_kmfitage$upper[1]
        upper_180_0 <- 1 - summary_kmfitage$lower[1]
      
        incidence_365_0 <- 1 - summary_kmfitage$surv[2]
        lower_365_0 <- 1 - summary_kmfitage$upper[2]
        upper_365_0 <- 1 - summary_kmfitage$lower[2]
        #18:
        incidence_180_18 <- 1 - summary_kmfitage$surv[3]
        lower_180_18 <- 1 - summary_kmfitage$upper[3]
        upper_180_18 <- 1 - summary_kmfitage$lower[3]
        
        incidence_365_18 <- 1 - summary_kmfitage$surv[4]
        lower_365_18 <- 1 - summary_kmfitage$upper[4]
        upper_365_18 <- 1 - summary_kmfitage$lower[4]
        #60:
        incidence_180_60 <- 1 - summary_kmfitage$surv[5]
        lower_180_60 <- 1 - summary_kmfitage$upper[5]
        upper_180_60 <- 1 - summary_kmfitage$lower[5]
        
        incidence_365_60 <- 1 - summary_kmfitage$surv[6]
        lower_365_60 <- 1 - summary_kmfitage$upper[6]
        upper_365_60 <- 1 - summary_kmfitage$lower[6]
            
    #Stratified cumulative incidence by sex and ageband:  
      #Fit a Kaplan-Meier stratified by sex and ageband:
      kmfitsexage <- survfit(Surv(days, flare)~sex_at_instance_creation+age_cat, data=input_dataset)
      #Survival at 180 and 365 days:
      summary_kmfitsexage <- summary(kmfitsexage, times=c(180,365), extend = T) 
      #Incidence and CI by sex and age:
      #Female & 0:
      incidence_180_F_0 <- 1 - summary_kmfitsexage$surv[1]
      lower_180_F_0 <- 1 - summary_kmfitsexage$upper[1]
      upper_180_F_0 <- 1 - summary_kmfitsexage$lower[1]
      
      incidence_365_F_0 <- 1 - summary_kmfitsexage$surv[2]
      lower_365_F_0 <- 1 - summary_kmfitsexage$upper[2]
      upper_365_F_0 <- 1 - summary_kmfitsexage$lower[2]
      
      #Female & 18:
      incidence_180_F_18 <- 1 - summary_kmfitsexage$surv[3]
      lower_180_F_18 <- 1 - summary_kmfitsexage$upper[3]
      upper_180_F_18 <- 1 - summary_kmfitsexage$lower[3]
      
      incidence_365_F_18 <- 1 - summary_kmfitsexage$surv[4]
      lower_365_F_18 <- 1 - summary_kmfitsexage$upper[4]
      upper_365_F_18 <- 1 - summary_kmfitsexage$lower[4]
  
      #Female & 60:
      incidence_180_F_60 <- 1 - summary_kmfitsexage$surv[5]
      lower_180_F_60 <- 1 - summary_kmfitsexage$upper[5]
      upper_180_F_60 <- 1 - summary_kmfitsexage$lower[5]
      
      incidence_365_F_60 <- 1 - summary_kmfitsexage$surv[6]
      lower_365_F_60 <- 1 - summary_kmfitsexage$upper[6]
      upper_365_F_60 <- 1 - summary_kmfitsexage$lower[6]  
      
      #Male & 0:
      incidence_180_M_0 <- 1 - summary_kmfitsexage$surv[7]
      lower_180_M_0 <- 1 - summary_kmfitsexage$upper[7]
      upper_180_M_0 <- 1 - summary_kmfitsexage$lower[7]
      
      incidence_365_M_0 <- 1 - summary_kmfitsexage$surv[8]
      lower_365_M_0 <- 1 - summary_kmfitsexage$upper[8]
      upper_365_M_0 <- 1 - summary_kmfitsexage$lower[8]
      
      #Male & 18:
      incidence_180_M_18 <- 1 - summary_kmfitsexage$surv[9]
      lower_180_M_18 <- 1 - summary_kmfitsexage$upper[9]
      upper_180_M_18 <- 1 - summary_kmfitsexage$lower[9]
      
      incidence_365_M_18 <- 1 - summary_kmfitsexage$surv[10]
      lower_365_M_18 <- 1 - summary_kmfitsexage$upper[10]
      upper_365_M_18 <- 1 - summary_kmfitsexage$lower[10]
      
      #Male & 60:
      incidence_180_M_60 <- 1 - summary_kmfitsexage$surv[11]
      lower_180_M_60 <- 1 - summary_kmfitsexage$upper[11]
      upper_180_M_60 <- 1 - summary_kmfitsexage$lower[11]
      
      incidence_365_M_60 <- 1 - summary_kmfitsexage$surv[12]
      lower_365_M_60 <- 1 - summary_kmfitsexage$upper[12]
      upper_365_M_60 <- 1 - summary_kmfitsexage$lower[12]
      
    #Create data frame with the cumulative incidence:
      incidence_df <- data.frame(
        period = c(1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2),
        age_band = c(99,99,0,0,18,18,60,60,99,99,99,99,0,0,0,0,18,18,18,18,60,60,60,60),
        gender = c("all","all","all","all","all","all","all","all","F","F","M","M","F","F","M","M","F","F","M","M","F","F","M","M"),
        ci = round(c(incidence_180,incidence_365,incidence_180_0,incidence_365_0,incidence_180_18,incidence_365_18,incidence_180_60,incidence_365_60,incidence_180_F,incidence_365_F,incidence_180_M,incidence_365_M,incidence_180_F_0,incidence_365_F_0,incidence_180_M_0,incidence_365_M_0,incidence_180_F_18,incidence_365_F_18,incidence_180_M_18,incidence_365_M_18,incidence_180_F_60,incidence_365_F_60,incidence_180_M_60,incidence_365_M_60),3),
        ll = round(c(lower_180,lower_365,lower_180_0,lower_365_0,lower_180_18,lower_365_18,lower_180_60,lower_365_60,lower_180_F,lower_365_F,lower_180_M,lower_365_M,lower_180_F_0,lower_365_F_0,lower_180_M_0,lower_365_M_0,lower_180_F_18,lower_365_F_18,lower_180_M_18,lower_365_M_18,lower_180_F_60,lower_365_F_60,lower_180_M_60,lower_365_M_60),3),
        ul = round(c(upper_180,upper_365,upper_180_0,upper_365_0,upper_180_18,upper_365_18,upper_180_60,upper_365_60,upper_180_F,upper_365_F,upper_180_M,upper_365_M,upper_180_F_0,upper_365_F_0,upper_180_M_0,upper_365_M_0,upper_180_F_18,upper_365_F_18,upper_180_M_18,upper_365_M_18,upper_180_F_60,upper_365_F_60,upper_180_M_60,upper_365_M_60),3)
      )
    #Export to excel file:
  #   write_xlsx(incidence_df, "D5_Table_7_cumulative_incidence.xlsx")
      
    # BELEN end
  }else{
    incidence_df <- data.table()
    survival_data_365 <- data.table()
  }
  
  #########################################
  # save

  outputfile <- as.data.table(incidence_df)
  nameoutput <- paste0("D5_Table_7_cumulative_incidence_",immdis)
  assign(nameoutput, outputfile)
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
  fwrite(outputfile,  file = file.path(thisdiroutput, paste0(nameoutput,".csv")))

  outputfile <- as.data.table(survival_data_365)
  nameoutput <- paste0("D5_data_KM_figure_",immdis)
  assign(nameoutput, outputfile)
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
  fwrite(outputfile,  file = file.path(thisdiroutput, paste0(nameoutput,".csv")))
}
