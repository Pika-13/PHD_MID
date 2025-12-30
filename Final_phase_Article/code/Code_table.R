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

#¡nstlall packages if not installed
list.of.packages <- c("tibble" ,"tidyverse", "dplyr", "data.table", "colorspace", "readxl", "haven",
                      "ggpubr", "ggbeeswarm", "scales", "gtsummary", 
                      "data.table", "flextable", "countrycode")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#loading packages from library
lapply(c("tibble" ,"tidyverse", "dplyr", "data.table", "colorspace", "readxl", "haven",
         "ggpubr", "ggbeeswarm", "scales", "gtsummary", 
         "data.table", "flextable", "countrycode"), 
       require,character.only = TRUE)

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


