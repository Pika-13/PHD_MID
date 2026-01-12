#This project is on the clinical purpose of research 
#It will be led by Joana Rodrigues Ribeiro and Fariba Abbassi
#And will be supervised by Milo A Puhan and Pierre Alain Clavien
#Will be submitted to Annals of Surgery,
#deadline article: February, 20 
#deadline data analysis: January (end)
#deadline data extraction: January 20-25th
#deadline screening + selection: January 5th
#-----------------------------------------------------
#Author: Joana RR
#Starting date: December 19, 2025
#Last update date: December X, 2025

#Protocol - sample size calculation based on 5 criteria (revised by MP):

#1.	Specification of the aim of the trial
#and/or
#3.	Specification of the aim of the study design (stats methods, sample size)

#AND all the following
#2.	Specification of the aim of a primary endpoint,
#4.	Description of a sample size calculation.
#5.	Use of a MID or clinically relevant effect to base sample size calculations upon
#6.	Consideration of the clinical relevance of results, e.g. using a MID (or terms associated with “clinical” or “patient” to discuss results for primary and/or secondary endpoints)

#1 and/or 3 = 100% (84/84)
#2 100%
#4 SS calc = 100% (84/84)
#5 studies with SS calculation with clinical relevant DELTA: 8/84 = 9.5%
#doi: 10.1097/SLA.0000000000006232  10.1007/s00586-023-07998-6, 
#   10.1097/SLA.0000000000006055    10.1097/SLA.0000000000001474
#   10.1016/j.bja.2020.06.020       10.1056/NEJMoa2100826
#   10.1001/jamasurg.2019.5474      10.1001/jamasurg.2021.1555

#6 studies with results discussing clinical relevance: 6/84 = 7.1%
#   10.1016/S2468-1253(24)00307-8   10.1200/JCO.23.01019
#   10.1016/j.bja.2020.06.020
#also (non significant p-value):
#   10.1007/s00384-025-04939-1   
#   10.1007/s00586-023-07998-6  
#   10.1097/SLA.0000000000006055

#total 5 criteria =2/84 = 2.4%
#   10.1097/SLA.0000000000006055
#   10.1016/j.bja.2020.06.020 


#¡nstlall packages if not installed
list.of.packages <- c("tibble" ,"tidyverse", "dplyr", "data.table", "colorspace", "readxl", "haven",
                      "ggpubr", "ggbeeswarm", "scales", "gtsummary", 
                      "data.table", "flextable", "countrycode", "presize")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#loading packages from library
lapply(c("tibble" ,"tidyverse", "dplyr", "data.table", "colorspace", "readxl", "haven",
         "ggpubr", "ggbeeswarm", "scales", "gtsummary", 
         "data.table", "flextable", "countrycode", "presize"), 
       require,character.only = TRUE)

#sample size
prec_prop(n=NULL, 0.02, conf.width = 0.05) # n=152
1612 led to 87

#create database
#database_v0 <- tibble(
#  id = integer(),
#  age = double(),
#  sex = factor(levels = c("F", "M")),
#  group = factor(levels = c("Control", "Treatment")),
#  cci = double()
#)
#-------------------------
#read csv from covidence
database_v0 <- read_csv("database/database_covidence.csv")
#country last author list
countries <- countrycode::codelist$country.name.en
countries <- sort(unique(countries))

#labels of variables
variable_list <- list(
  author = "First author",
  year = "Year of publication",
  title = "Title",
  journal = "Journal",
  study_design = "Study design",
  country = "Country of last author",
 # multi_center = "Multicentric study",
  RCT_clinical_methods_primary_endpoint = "Choice of primary endpoint based on clinical relevance",
  RCT_clinical_methods_sample = "Calculation of sample size based on clinical relevance",
  RCT_methods_primary_endpoint = "Primary endpoint",
  RCT_methods_secondary_endpoint = "Secondary endpoint",
  RCT_methods_sample = "Sample size calculation",
  RCT_results_p_value = "Significance of results",
  RCT_clinical_discussion_interpretation = "Interpretation of results based on clinical relevance",
  Pretrial_clinical_primary_endpoint = "Pretrial documentation: Choice of primary endpoint based on clinical relevance",
  Pretrial_clinical_methods_sample = "Pretrial documentation: Calculation of sample size based on clinical relevance"
)

#add columns
database_v1 <- database_v0 %>%
  mutate(
    author = as.character(),
    year = integer() ,
    title = as.character(),
    journal = as.character(),
    study_design = factor(levels = c(0:2,999), 
                          labels=c("Superiority", "Non-inferiority", 
                                   "Equivalence", "Not reported")),
    country = factor(levels = countries),
  #  multi_center = NA_character_,
   RCT_clinical_methods_primary_endpoint = factor(levels=c(0:1),
                                                  labels =c("Not addressed", "Addressed")),
  RCT_clinical_methods_sample = factor(levels=c(0:1),
                                       labels =c("Not addressed", "Addressed")),
 RCT_methods_primary_endpoint = as.character(),
  RCT_methods_secondary_endpoint = as.character(),
  RCT_methods_sample = as.character(),
  RCT_results_p_value = factor(levels=c(0:1),
                           labels =c("Non-statistically significant", "Statistically significant")),
  RCT_clinical_discussion_interpretation = factor(levels=c(0:1),
                                         labels =c("Not addressed", "Addressed")),
  Pretrial_clinical_methods_sample = factor(levels=c(0:1),
                                       labels =c("Not addressed", "Addressed")),
  Pretrial_clinical_methods_outcome = factor(levels=c(0:1),
                                             labels =c("Not addressed", "Addressed")),
  Pretrial_clinical_methods_sample = as.character(),
  )


