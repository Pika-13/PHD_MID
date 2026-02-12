#Author: Joana RR
#Starting date: January th, 2025
#Last update date: January , 2025
#--------------------------------------------
#install packages if not installed
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
#--------------------------------------------
#read tidied dataset
  database_v2 <- read_csv("Final_phase_Article/database/database_covidence_v1.csv") %>% 
  mutate(outcome = factor(outcome,levels=c(1,0),
                          labels =c("Fulfilling criteria","Not fulfilling criteria")),
         condordance_aim = factor(condordance_aim,levels=c(1,0,2),
                                  labels =c("Yes ", "No ", "Discordant trial aim between pretrial document and final RCT")),
         condordance_primary_endpoint= factor(condordance_primary_endpoint,levels=c(1,0,2, 999),
                                              labels =c("Yes", "No", "Discordant endpoints between pretrial document and final RCT",
                                                        "Not available")),
         condordance_design_aim= factor(condordance_design_aim,levels=c(1,0,3,999), 
                                        labels =c("Yes", "No",
                                                  "Discordant design aim/ reported aim between pretrial document and final RCT",
                                                  "Not available")),
         condordance_sample_size= factor(condordance_sample_size,levels=c(1,0,2,999),
                                         labels =c("Yes", "No","Discordant delta between pretrial document and final RCT", "Not available")),
         RCT_clinical_discussion_interpretation= factor(RCT_clinical_discussion_interpretation,
                                                              levels=c(1,0,999),
                                                              labels =c("Yes", "No", "Not available")),
         final_publication_significant_results=factor(final_publication_significant_results,
                                          levels=c(1,0,999),
                                          labels =c("Yes", "No", "Not available")),
         pretrial_document_type = factor(pretrial_document_type,levels=c(1,2,3,999),
         labels =c("Published protocol", "Unpublished protocol", "Registry only", "Not available" ))
  )


#specifying factor variables
#ensure factor variables are factors
discrete_variables <- c(
  "outcome",
  "condordance_aim",
  "condordance_primary_endpoint",
  "condordance_design_aim",
  "condordance_sample_size",
  "RCT_clinical_discussion_interpretation",
  "final_publication_significant_results",
  "pretrial_document_type")
database_v2[, discrete_variables] <- lapply(
  database_v2[, discrete_variables], factor)

#create labels
labels_table_1 <- list(
   "outcome"~"RCTs fulfilling at least four criteria",
   "condordance_aim"~"Study hypothesis specified",
   "condordance_design_aim"~"Trial design aligned with study hypothesis",
   "condordance_primary_endpoint"~"Primary endpoint specified",
   "condordance_sample_size"~"Sample size calculation based on clinical relevance",
   "RCT_clinical_discussion_interpretation"~"Results interpreted based on clinical relevance"
   #,"final_publication_significant_results"~"Statistical significance achieved for primary endpoint"
  #,pretrial_document_type ~ "Pretrial documentation type"
)

#create tbl_summary no comparison
summary_table_1 <- database_v2 %>%
  select(
    outcome,
    condordance_aim,
    condordance_design_aim,
    condordance_primary_endpoint,
    condordance_sample_size,
    RCT_clinical_discussion_interpretation
    #,final_publication_results
    #,pretrial_documentation_type
  ) %>%
  tbl_summary(
    label = labels_table_1,
    statistic = all_categorical() ~ "{n} ({p}%)" ,
    missing = "no",
    digits = list(
      all_categorical() ~ c(0,1,0))
  ) %>% 
  as_flex_table() %>% 
  add_footer_lines(values = "Results are expressed in numbers (percentage(%))" ) %>% 
  add_footer_lines(values ="Abbreviations: RCT, Randomized controlled trial"
  ) %>% 
  set_caption("Table 1. Assessment of ability of surgical RCTs to meaningfully inform clinical practice")
#save
save_as_docx(summary_table_1, path = "Final_phase_Article/output/tables/Table_1_Assessment_surgical_RCTs_clinical_practice.docx")

#create tbl_summary 2 comparing RCTs with registry documentation/unpublished protocol/published protocol
summary_table <- 
