#project on MID with Fariba
#Summary table with matches mismatches

#Date creation: 2025-10-07
#Last update: 2025-10-...

#Load necessary packages
lapply(c("tidyverse", "data.table","utils", "writexl", "readxl",
         "ggpubr", "scales", "flextable", "janitor", "labelled", "gtsummary", "dplyr", "markdown"), require,
       character.only = TRUE)

# Open long_RCT_matched_publications_protocols
long_RCT_matched_publications_protocols <- read_csv("database/csv_files_R_coding/long_RCT_matched_publications_protocols_2025_10_10.csv")
#----------------------------------------------
#ensure factor variables are factors
discrete_variables <- c(#"paper_type", 
                        "protocol",
                        #"protocol_published_original_column",
                        "study_type_pilot",
                        "pooled_analysis",
                        "post_hoc_secondary_analysis",
                        "discipline",
                        "continent",
                        "cci_ep",
                        "cci_1st_ep",
                        "cci_2nd_ep",
                        "cci_expl_ep",
                        "cd_cci_reported_in_results",
                        "cci_modification",
                        "primary_outcome_significant",
                        "statistically_significant_difference_for_cci",
                        "between_group_difference_of_cci_12",
                        "mean_or_median_used",
                        "study_type_mentioned", 
                        "type_of_study_sup_non_inf",
                        "ss_calculation", 
                        "cci_used_for_ss_calculation",
                        #"specification_of_ss_calculation",
                        "non_inf_margin",
                        #"definition_of_non_inf_margin",
                        "protocol_published")
long_RCT_matched_publications_protocols[, discrete_variables] <- lapply(
  long_RCT_matched_publications_protocols[, discrete_variables], factor)

#ensure all continuous variables are numeric
continuous_variables <- c("sample_size", "between_group_difference_of_cci")
long_RCT_matched_publications_protocols[, continuous_variables] <- lapply(
  long_RCT_matched_publications_protocols[, continuous_variables], as.numeric)

#ensure factor variables are ordered
long_RCT_matched_publications_protocols$protocol <- factor(long_RCT_matched_publications_protocols$protocol, 
                                     levels = c("Protocol", "Publication"))
long_RCT_matched_publications_protocols$protocol_published <- factor(long_RCT_matched_publications_protocols$protocol_published, 
                                               levels = c("Protocol published", 
                                                          "Protocol unpublished",
                                                          "No protocol,registry"))
long_RCT_matched_publications_protocols$study_type_pilot <- factor(long_RCT_matched_publications_protocols$study_type_pilot, 
                                         levels = c("Pilot/ Safety study", "Not pilot study"))
long_RCT_matched_publications_protocols$pooled_analysis <- factor(long_RCT_matched_publications_protocols$pooled_analysis, 
                                      levels = c("Yes", "No"),labels = c("Yes", "No"))
long_RCT_matched_publications_protocols$post_hoc_secondary_analysis <- factor(long_RCT_matched_publications_protocols$post_hoc_secondary_analysis, 
                                               levels = c("Yes", "No"),labels = c("Yes", "No"))
long_RCT_matched_publications_protocols$cci_ep <- factor(long_RCT_matched_publications_protocols$cci_ep, 
                              levels = c("CCI primary endpoint", 
                                         "CCI non-primary endpoint",
                                         "CCI not an endpoint"))
long_RCT_matched_publications_protocols$cci_1st_ep = factor(long_RCT_matched_publications_protocols$cci_1st_ep,
                    levels = c("No", "Yes"),labels = c("Yes", "No"))
long_RCT_matched_publications_protocols$cci_2nd_ep = factor(long_RCT_matched_publications_protocols$cci_2nd_ep,
                    levels = c("No", "Yes"),labels = c("Yes", "No"))
long_RCT_matched_publications_protocols$cci_expl_ep = factor(long_RCT_matched_publications_protocols$cci_expl_ep,
                     levels =  c("No", "Yes"),labels = c("Yes", "No"))
long_RCT_matched_publications_protocols$cd_cci_reported_in_results <- factor(long_RCT_matched_publications_protocols$cd_cci_reported_in_results, 
                                       levels = c("Yes", "No"),labels = c("Yes", "No"))
  long_RCT_matched_publications_protocols$cci_modification <- factor(long_RCT_matched_publications_protocols$cci_modification, 
                                       levels = c("Yes", "No"),labels = c("Yes", "No"))
  long_RCT_matched_publications_protocols$primary_outcome_significant <- factor(long_RCT_matched_publications_protocols$primary_outcome_significant, 
                                       levels = c("Yes", "No"),labels = c("Yes", "No"))
  long_RCT_matched_publications_protocols$statistically_significant_difference_for_cci <- factor(long_RCT_matched_publications_protocols$statistically_significant_difference_for_cci, 
                                       levels = c("Yes", "No"),labels = c("Yes", "No"))
  long_RCT_matched_publications_protocols$between_group_difference_of_cci_12 <- factor(long_RCT_matched_publications_protocols$between_group_difference_of_cci_12, 
                                       levels = c("Yes", "No"),labels = c("Yes", "No"))
  long_RCT_matched_publications_protocols$mean_or_median_used <- factor(long_RCT_matched_publications_protocols$mean_or_median_used, 
                                       levels = c("Mean", "Median"),labels = c("Yes", "No"))
    long_RCT_matched_publications_protocols$study_type_mentioned <- factor(long_RCT_matched_publications_protocols$study_type_mentioned, 
                                       levels = c("Yes", "No"),labels = c("Yes", "No"))
    long_RCT_matched_publications_protocols$type_of_study_sup_non_inf <- factor(long_RCT_matched_publications_protocols$type_of_study_sup_non_inf,
                                                                                levels = c("Non-inferiority trial (explicited)", "Non-inferiority trial (assumed)",
                                                                                           "Superiority trial (explicited)", "Superiority trial (assumed)",
                                                                                           "Not mentioned"),
                                                                                labels = c("Non-inferiority trial (explicited)", "Non-inferiority trial (assumed)",
                                                                                           "Superiority trial (explicited)", "Superiority trial (assumed)",
                                                                                           "Not mentioned"))
    long_RCT_matched_publications_protocols$ss_calculation <- factor(long_RCT_matched_publications_protocols$ss_calculation, 
                                       levels = c("Yes", "No"),labels = c("Yes", "No"))
    long_RCT_matched_publications_protocols$cci_used_for_ss_calculation <- factor(long_RCT_matched_publications_protocols$cci_used_for_ss_calculation, 
                                       levels = c("Yes", "No"),labels = c("Yes", "No"))
    long_RCT_matched_publications_protocols$non_inf_margin <- factor(long_RCT_matched_publications_protocols$non_inf_margin, 
                                       levels = c("Yes", "No"),labels = c("Yes", "No"))


var_list_matched <- list(
  # paper_type ~"Paper type", 
  #protocol ~"Protocol", 
  protocol_published ~"Protocol publication status",
  #protocol_published_original_columnn~"Protocol publication status", 
  #study_type_pilot ~"Pilot/ feasibility study",
  #pooled_analysis ~"Pooled analysis",
  #post_hoc_secondary_analysis ~"Post-hoc secondary analysis",
 # discipline ~"Discipline",
 # continent ~"Continent", 
  # sample_size ~ "Sample size",
  cci_ep ~"CCI use as endpoint in paper",
  # cd_cci_reported_in_results ~ "CCI report in results",
  #cci_modification ~"CCI modification",
  #specification_of_modification ~"CCI modification specification",
  # primary_outcome_significant ~ "Primary outcome significant",
  # statistically_significant_difference_for_cci ~"CCI difference significant",
  # between_group_difference_of_cci_12~"CCI difference ≥ 12 pts",
  #  between_group_difference_of_cci ~"CCI difference [pts]",
  # mean_or_median_used ~ "Mean or median used",
  type_of_study_sup_non_inf ~"Trial type (superiority/ non-inferiority)", 
  ss_calculation ~"Sample size calculation",
  cci_used_for_ss_calculation ~ "CCI used for sample size calculation",
  #specification_of_ss_calculation ~"Details on sample size calculation",
  non_inf_margin~"Non-inferiority margin definition"
  #definition_of_non_inf_margin ~"Details on non-inferiority margin",
  # protocol_published ~"Protocol publication status")
)

#table code
Table_descriptive_long_RCT_matched_publications_protocols <- long_RCT_matched_publications_protocols %>%
  select(protocol, protocol_published, cci_ep, 
         #study_type_mentioned,
         type_of_study_sup_non_inf,ss_calculation,
         cci_used_for_ss_calculation,non_inf_margin) %>% 
  tbl_summary(
    by = protocol,
    label = var_list_matched,
    statistic = list(
     # c("sample_size", "between_group_difference_of_cci") ~ "{median} ({p25}-{p75})",
      c("protocol_published", "cci_ep",
        #"study_type_mentioned",
             "type_of_study_sup_non_inf",
             "ss_calculation", "cci_used_for_ss_calculation",
             "non_inf_margin") ~ "{p}% ({n}/{N_nonmiss})"),
    , missing = "no",
    digits = list(
      all_categorical() ~ c(1,0,0),
      all_continuous() ~ c(2,2,2)# 0 decimals for counts, 1 for percentages
    ))
  #add_overall()
  #add_p() %>%

#Save Table_descriptive
Table_descriptive_long_RCT_matched_publications_protocols_path<-paste(getwd(), "/tables/Table_descriptive_long_RCT_matched_publications_protocols_2025_10_10.docx", sep = "")
Table_descriptive_long_RCT_matched_publications_protocols%>%as_flex_table()%>%
  flextable::save_as_docx(Table_descriptive_long_RCT_matched_publications_protocols, 
                          path = "tables/Table_descriptive_long_RCT_matched_publications_protocols_2025_10_10.docx")








#-----------------------------------
#Mismatches
#open table with _protocol variables
RCT_matched_publications_protocols <- read_csv("database/csv_files_R_coding/RCT_matched_publications_protocols_2025_10_10.csv") 
  RCT_matched_publications_protocols$cci_ep <- factor(RCT_matched_publications_protocols$cci_ep, 
                                               levels = c("CCI primary endpoint", 
                                                          "CCI non-primary endpoint"
                                                          #,"CCI not an endpoint"
                                                          ))
RCT_matched_publications_protocols$cci_ep_protocol <- factor(RCT_matched_publications_protocols$cci_ep_protocol, 
                                                    levels = c("CCI primary endpoint", 
                                                               "CCI non-primary endpoint",
                                                              "CCI not an endpoint"))
mismatch_table <- 
  RCT_matched_publications_protocols %>%
  select(cci_ep,cci_ep_protocol) %>% 
  tbl_summary(
    by = cci_ep ,
    label = cci_ep_protocol ~"CCI as an endpoint in pre-trial documentation",
    statistic = list(
      cci_ep_protocol ~ "{p}% ({n}/{N_nonmiss})"),
    digits = list(
      all_categorical() ~ c(1,0,0)
     # all_continuous() ~ c(2,2,2)# 0 decimals for counts, 1 for percentages
    ), missing = "no"
  ) %>% 
 # add_overall(last=TRUE) %>% 
  as_flex_table()      %>% 
  add_header_row(
    values = c("", "CCI as an endpoint in published trial"),# Adjust number of columns
    colwidths = c(1, 2)                       # Set number of columns under each spanner
  ) %>% 
  bold(part = "header", i=1) %>% 
  bold(part="body", j=1) 

  save_as_docx(mismatch_table, path = "tables/mismatch_table_2025_10_10.docx")
  
  
  
  
  
  
  
  
  
  
  
#-------------------------------------------------
#general table with all 231 papers
#List with all renamed variables
var_list_total <- list(
 # paper_type ~"Paper type", 
   #protocol ~"Protocol", 
   protocol_published ~"Protocol publication",
  #protocol_published_original_columnn~"Protocol publication status", 
  #study_type_pilot ~"Pilot/ feasibility study",
  pooled_analysis ~"Pooled analysis",
  post_hoc_secondary_analysis ~"Post-hoc secondary analysis",
  discipline ~"Discipline",
  continent ~"Continent", 
 # sample_size ~ "Sample size",
  cci_ep ~"CCI use as endpoint in paper",
 # cd_cci_reported_in_results ~ "CCI report in results",
  #cci_modification ~"CCI modification",
  #specification_of_modification ~"CCI modification specification",
 # primary_outcome_significant ~ "Primary outcome significant",
 # statistically_significant_difference_for_cci ~"CCI difference significant",
 # between_group_difference_of_cci_12~"CCI difference ≥ 12 pts",
#  between_group_difference_of_cci ~"CCI difference [pts]",
 # mean_or_median_used ~ "Mean or median used",
  type_of_study_sup_non_inf ~"Trial type (superiority/ non-inferiority)", 
  ss_calculation ~"Sample size calculation",
  cci_used_for_ss_calculation ~ "CCI used for sample size calculation",
  #specification_of_ss_calculation ~"Details on sample size calculation",
  non_inf_margin~"Non-inferiority margin definition")
  #definition_of_non_inf_margin ~"Details on non-inferiority margin")

#read 231 rct table
long_231_RCT_protocols_and_or_trials <- read_csv("database/csv_files_R_coding/long_231_RCT_protocols_and_or_trials_2025_10_10.csv")

#ensure factor variables are factors
discrete_variables <- c(#"paper_type", 
  "protocol",
  #"protocol_published_original_column",
  "study_type_pilot",
  "pooled_analysis",
  "post_hoc_secondary_analysis",
  "discipline",
  "continent",
  "protocol_published",
  "cci_ep",
  "cci_1st_ep",
  "cci_2nd_ep",
  "cci_expl_ep",
  "cd_cci_reported_in_results",
  "cci_modification",
  "primary_outcome_significant",
  "statistically_significant_difference_for_cci",
  "between_group_difference_of_cci_12",
  "mean_or_median_used",
  "study_type_mentioned", 
  "significance_considered",
  "conclusion_of_authors",
  "type_of_study_sup_non_inf",
  "ss_calculation", 
  "cci_used_for_ss_calculation",
  #"specification_of_ss_calculation",
  "non_inf_margin"
  #"definition_of_non_inf_margin"
  )
long_231_RCT_protocols_and_or_trials[, discrete_variables] <- lapply(
  long_231_RCT_protocols_and_or_trials[, discrete_variables], factor)

#ensure all continuous variables are numeric
continuous_variables <- c("sample_size", "between_group_difference_of_cci")
long_231_RCT_protocols_and_or_trials[, continuous_variables] <- lapply(
  long_231_RCT_protocols_and_or_trials[, continuous_variables], as.numeric)

#ensure factor variables are ordered
long_231_RCT_protocols_and_or_trials$protocol <- factor(long_231_RCT_protocols_and_or_trials$protocol, 
                                                           levels = c("Publication","Protocol"),
                                                        labels = c("Published trial with respective pre-trial documentation", "Pre-trial documentation with respective published trial"))
long_231_RCT_protocols_and_or_trials$protocol_published <- factor(long_231_RCT_protocols_and_or_trials$protocol_published, 
                                                                     levels = c("Protocol published", 
                                                                                "Protocol unpublished",
                                                                                "No protocol,registry", "Neither protocol nor registry"))
long_231_RCT_protocols_and_or_trials$study_type_pilot <- factor(long_231_RCT_protocols_and_or_trials$study_type_pilot, 
                                                                   levels = c("Pilot/ Safety study", "Not pilot study"))
long_231_RCT_protocols_and_or_trials$pooled_analysis <- factor(long_231_RCT_protocols_and_or_trials$pooled_analysis, 
                                                                  levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
long_231_RCT_protocols_and_or_trials$post_hoc_secondary_analysis <- factor(long_231_RCT_protocols_and_or_trials$post_hoc_secondary_analysis, 
                                                                              levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
long_231_RCT_protocols_and_or_trials$cci_ep <- factor(long_231_RCT_protocols_and_or_trials$cci_ep, 
                                                         levels = c("CCI primary endpoint", 
                                                                    "CCI non-primary endpoint",
                                                                    "CCI not an endpoint"))
long_231_RCT_protocols_and_or_trials$cci_1st_ep = factor(long_231_RCT_protocols_and_or_trials$cci_1st_ep,
                                                            levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
long_231_RCT_protocols_and_or_trials$cci_2nd_ep = factor(long_231_RCT_protocols_and_or_trials$cci_2nd_ep,
                                                            levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
long_231_RCT_protocols_and_or_trials$cci_expl_ep = factor(long_231_RCT_protocols_and_or_trials$cci_expl_ep,
                                                             levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
long_231_RCT_protocols_and_or_trials$cd_cci_reported_in_results <- factor(long_231_RCT_protocols_and_or_trials$cd_cci_reported_in_results, 
                                                                             levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
long_231_RCT_protocols_and_or_trials$cci_modification <- factor(long_231_RCT_protocols_and_or_trials$cci_modification, 
                                                                   levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
long_231_RCT_protocols_and_or_trials$primary_outcome_significant <- factor(long_231_RCT_protocols_and_or_trials$primary_outcome_significant, 
                                                                              levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
long_231_RCT_protocols_and_or_trials$statistically_significant_difference_for_cci <- factor(long_231_RCT_protocols_and_or_trials$statistically_significant_difference_for_cci, 
                                                                                               levels = c("Yes", "No"),labels = c("Yes", "No"))
long_231_RCT_protocols_and_or_trials$between_group_difference_of_cci_12 <- factor(long_231_RCT_protocols_and_or_trials$between_group_difference_of_cci_12, 
                                                                                     levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
long_231_RCT_protocols_and_or_trials$mean_or_median_used <- factor(long_231_RCT_protocols_and_or_trials$mean_or_median_used, 
                                                                      levels = c("Mean", "Median", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
long_231_RCT_protocols_and_or_trials$study_type_mentioned <- factor(long_231_RCT_protocols_and_or_trials$study_type_mentioned, 
                                                                       levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
long_231_RCT_protocols_and_or_trials$type_of_study_sup_non_inf <- factor(long_231_RCT_protocols_and_or_trials$type_of_study_sup_non_inf,
                                                                         levels = c( "Superiority trial (explicited)",     "Superiority trial (assumed)", "Non-inferiority trial (explicited)", "Non-inferiority trial (assumed)",   
                                                                                     "Equivalence trial", "Not mentioned"))   
long_231_RCT_protocols_and_or_trials$ss_calculation <- factor(long_231_RCT_protocols_and_or_trials$ss_calculation, 
                                                                 levels = c("Yes", "No"),labels = c("Yes", "No"))
long_231_RCT_protocols_and_or_trials$cci_used_for_ss_calculation <- factor(long_231_RCT_protocols_and_or_trials$cci_used_for_ss_calculation, 
                                                                              levels = c("Yes", "No", "Not mentioned"))
long_231_RCT_protocols_and_or_trials$non_inf_margin <- factor(long_231_RCT_protocols_and_or_trials$non_inf_margin, 
                                                                 levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
RCT_cci_1_ep$significance_considered <- factor(RCT_cci_1_ep$significance_considered, 
                                               levels = c("Statistical significance", "Both statistical and clinical significance"))
RCT_cci_1_ep$conclusion_of_authors <- factor(RCT_cci_1_ep$conclusion_of_authors, 
                                                                 levels = c("Favorable to intervention",
                                                                            "Neutral", "Not mentioned"," Unfavorable to intervention"))


#table code
         

Table_descriptive_long_231_RCT_protocols_and_or_trials <- long_231_RCT_protocols_and_or_trials %>% 
  select(protocol, 
         protocol_published ,
         #study_type_pilot ,
         pooled_analysis,
         post_hoc_secondary_analysis,
          continent, discipline, 
         # sample_size,
         cci_ep ,
         #primary_outcome_significant ,
         #statistically_significant_difference_for_cci ,
         # between_group_difference_of_cci_12,
         # between_group_difference_of_cci,
         # mean_or_median_used,
         #  study_type_mentioned,
         type_of_study_sup_non_inf,
         ss_calculation,
         cci_used_for_ss_calculation,
         non_inf_margin
  ) %>% 
  tbl_summary(
   # by = protocol,
    label = var_list_total,
    statistic = list(
      # c("between_group_difference_of_cci",
      #"sample_size") ~ "{median} ({p25}-{p75}), ( {N_nonmiss})",
      c( "protocol_published" ,
         #"study_type_pilot" ,
         "pooled_analysis",
         "post_hoc_secondary_analysis",
         "continent",
         "discipline",
         "cci_ep" ,
         #"primary_outcome_significant" ,
         # "statistically_significant_difference_for_cci" ,
         #"between_group_difference_of_cci_12",
         # "mean_or_median_used",
         "type_of_study_sup_non_inf",
         "ss_calculation",
         "cci_used_for_ss_calculation","non_inf_margin")  ~ "{p}% ({n}/{N_nonmiss})"),
    missing = "no",
    digits = list(
      all_categorical() ~ c(1,0,0),
      all_continuous() ~ c(0,0,0)# 0 decimals for counts, 1 for percentages
     ))
#add_overall()
#add_p() %>%
#Save Table_descriptive
Table_descriptive_long_231_RCT_protocols_and_or_trials_path<-paste(getwd(), "/tables/Table_descriptive_long_231_RCT_protocols_and_or_trials_2025_10_10.docx", sep = "")
Table_descriptive_long_231_RCT_protocols_and_or_trials%>%as_flex_table()%>%
  flextable::save_as_docx(Table_descriptive_long_231_RCT_protocols_and_or_trials, 
                          path = "tables/Table_descriptive_long_231_RCT_protocols_and_or_trials_2025_10_10.docx")







#-----------------------------------

#table with cci = 1st ep
RCT_cci_1_ep <- read_csv("database/csv_files_R_coding/RCT_cci_1_ep_2025_10_10.csv" )
#List with all renamed variables
var_list_cci_1_ep <- list(
  # paper_type ~"Paper type", 
 # protocol ~"Protocol", 
  protocol_published ~"Protocol publication",
  #protocol_published_original_columnn~"Protocol publication status", 
  #study_type_pilot ~"Pilot/ feasibility study",
 # pooled_analysis ~"Pooled analysis",
  post_hoc_secondary_analysis ~"Post-hoc secondary analysis",
 # discipline ~"Discipline",
 # continent ~"Continent", 
   sample_size ~ "Sample size",
 # cci_ep ~"CCI use as endpoint in paper",
  # cd_cci_reported_in_results ~ "CCI report in results",
 # cci_modification ~"CCI modification",
  #specification_of_modification ~"CCI modification specification",
   primary_outcome_significant ~ "Primary outcome significant",
   statistically_significant_difference_for_cci ~"CCI difference significant",
   between_group_difference_of_cci_12~"CCI difference ≥ 12 pts",
    between_group_difference_of_cci ~"CCI difference [pts]",
   mean_or_median_used ~ "Mean or median used",
  type_of_study_sup_non_inf ~"Trial type (superiority/ non-inferiority)", 
  ss_calculation ~"Sample size calculation",
  cci_used_for_ss_calculation ~ "CCI used for sample size calculation",
  #specification_of_ss_calculation ~"Details on sample size calculation",
  non_inf_margin~"Non-inferiority margin definition",
  #definition_of_non_inf_margin ~"Details on non-inferiority margin",
  protocol_published ~"Protocol publication status",
  significance_considered ~"Significance considered",
  conclusion_of_authors ~"Conclusion favours intervention"
  )

#ensure factor ordered values
#ensure factor variables are factors
discrete_variables <- c(#"paper_type", 
  "protocol",
  #"protocol_published_original_column",
  "study_type_pilot",
  "pooled_analysis",
  "post_hoc_secondary_analysis",
  "discipline",
  "continent",
  "protocol_published",
  "cci_ep",
  "cci_1st_ep",
  "cci_2nd_ep",
  "cci_expl_ep",
  "cd_cci_reported_in_results",
  "cci_modification",
  "primary_outcome_significant",
  "statistically_significant_difference_for_cci",
  "between_group_difference_of_cci_12",
  "mean_or_median_used",
  "study_type_mentioned", 
  "significance_considered",
  "conclusion_of_authors",
  "type_of_study_sup_non_inf",
  "ss_calculation", 
  "cci_used_for_ss_calculation",
  #"specification_of_ss_calculation",
  "non_inf_margin"
  #"definition_of_non_inf_margin"
)
RCT_cci_1_ep[, discrete_variables] <- lapply(
  RCT_cci_1_ep[, discrete_variables], factor)

#ensure all continuous variables are numeric
continuous_variables <- c("sample_size", "between_group_difference_of_cci")
RCT_cci_1_ep[, continuous_variables] <- lapply(
  RCT_cci_1_ep[, continuous_variables], as.numeric)

#ensure factor variables are ordered
RCT_cci_1_ep$protocol <- factor(RCT_cci_1_ep$protocol,levels = c("Not a protocol"), labels = c("Published trial"))
RCT_cci_1_ep$protocol_published <- factor(RCT_cci_1_ep$protocol_published, 
                                                                  levels = c("Protocol published", 
                                                                             "Protocol unpublished",
                                                                             "No protocol,registry", "Neither protocol nor registry"))
RCT_cci_1_ep$study_type_pilot <- factor(RCT_cci_1_ep$study_type_pilot, 
                                                                levels = c("Pilot/ Safety study", "Not pilot study"))
RCT_cci_1_ep$pooled_analysis <- factor(RCT_cci_1_ep$pooled_analysis, 
                                                               levels = c("Yes", "No"))
RCT_cci_1_ep$post_hoc_secondary_analysis <- factor(RCT_cci_1_ep$post_hoc_secondary_analysis, 
                                                                           levels = c("Yes", "No"))
RCT_cci_1_ep$cci_ep <- factor(RCT_cci_1_ep$cci_ep, 
                                                      levels = c("CCI primary endpoint", 
                                                                 "CCI non-primary endpoint",
                                                                 "CCI not an endpoint"))
RCT_cci_1_ep$cci_1st_ep = factor(RCT_cci_1_ep$cci_1st_ep,
                                                         levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
RCT_cci_1_ep$cci_2nd_ep = factor(RCT_cci_1_ep$cci_2nd_ep,
                                                         levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
RCT_cci_1_ep$cci_expl_ep = factor(RCT_cci_1_ep$cci_expl_ep,
                                                          levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
RCT_cci_1_ep$cd_cci_reported_in_results <- factor(RCT_cci_1_ep$cd_cci_reported_in_results, 
                                                                          levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
RCT_cci_1_ep$primary_outcome_significant <- factor(RCT_cci_1_ep$primary_outcome_significant, 
                                                                           levels = c("Yes", "No"))
RCT_cci_1_ep$statistically_significant_difference_for_cci <- factor(RCT_cci_1_ep$statistically_significant_difference_for_cci, 
                                                                                            levels = c("Yes", "No"),labels = c("Yes", "No"))
RCT_cci_1_ep$between_group_difference_of_cci_12 <- factor(RCT_cci_1_ep$between_group_difference_of_cci_12, 
                                                                                  levels = c("Yes", "No"),labels = c("Yes", "No"))
RCT_cci_1_ep$mean_or_median_used <- factor(RCT_cci_1_ep$mean_or_median_used, 
                                                                   levels = c("Mean", "Median", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
RCT_cci_1_ep$study_type_mentioned <- factor(RCT_cci_1_ep$study_type_mentioned, 
                                                                    levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
RCT_cci_1_ep$type_of_study_sup_non_inf <- factor(RCT_cci_1_ep$type_of_study_sup_non_inf,
                                                                         levels = c( "Superiority trial (explicited)",     "Superiority trial (assumed)", "Non-inferiority trial (explicited)", "Non-inferiority trial (assumed)"   
                                                                                              ))   
RCT_cci_1_ep$ss_calculation <- factor(RCT_cci_1_ep$ss_calculation, 
                                                              levels = c("Yes", "No"),labels = c("Yes", "No"))
RCT_cci_1_ep$cci_used_for_ss_calculation <- factor(RCT_cci_1_ep$cci_used_for_ss_calculation, 
                                                                           levels = c("Yes", "No"))
RCT_cci_1_ep$non_inf_margin <- factor(RCT_cci_1_ep$non_inf_margin, 
                                                              levels = c("Yes", "No"),labels = c("Yes", "No"))
RCT_cci_1_ep$significance_considered <- factor(RCT_cci_1_ep$significance_considered, 
                                               levels = c("Statistical significance", "Both statistical and clinical significance"))
RCT_cci_1_ep$conclusion_of_authors <- factor(RCT_cci_1_ep$conclusion_of_authors, 
                                             levels = c("Favorable to intervention",
                                                        "Neutral","Unfavorable to intervention", "Not mentioned"))

#table code
Table_descriptive_RCT_cci_1_ep <- RCT_cci_1_ep %>% 
  select(
    protocol_published ,
    #study_type_pilot ,pooled_analysis,
         post_hoc_secondary_analysis,
        # continent, discipline, 
         sample_size,
      #   cci_ep ,
    #cci_modification,
         primary_outcome_significant ,
    between_group_difference_of_cci,
    mean_or_median_used,
    between_group_difference_of_cci_12,
    statistically_significant_difference_for_cci ,
    significance_considered,
    conclusion_of_authors,
         #  study_type_mentioned,
         type_of_study_sup_non_inf,
         ss_calculation,
         cci_used_for_ss_calculation,
         non_inf_margin
  ) %>% 
  tbl_summary(
    #by = protocol,
    label = var_list_cci_1_ep,
    statistic = list(
       c("between_group_difference_of_cci",
      "sample_size") ~ "{median} ({p25}-{p75}), ({N_nonmiss})",
      c(
        #"protocol",
        "protocol_published" ,
        #"study_type_pilot" ,"pooled_analysis",
        "post_hoc_secondary_analysis",
        #"cci_ep" ,
        "primary_outcome_significant" ,
       # "cci_modification",
         "statistically_significant_difference_for_cci" ,
        "between_group_difference_of_cci_12",
         "mean_or_median_used",
        "type_of_study_sup_non_inf",
        "ss_calculation",
        "cci_used_for_ss_calculation","non_inf_margin",
        "significance_considered", "conclusion_of_authors")  ~ "{p_nonmiss}% ({n}/{N_nonmiss})"),
    missing = "no" ,
    digits = list(
      all_categorical() ~ c(1,0,0),
      sample_size ~ c(0,0,0),# 0 decimals for counts, 1 for percentages
      between_group_difference_of_cci ~ c(1,1,0)# 0 decimals for counts, 1 for percentages
     )) 
#add_overall()
#add_p() %>%
#Save Table_descriptive
Table_descriptive_RCT_cci_1_ep_path<-paste(getwd(), "/tables/Table_descriptive_RCT_cci_1_ep_2025_10_10", sep = "")
Table_descriptive_RCT_cci_1_ep%>%as_flex_table()%>%
  flextable::save_as_docx(Table_descriptive_RCT_cci_1_ep, 
                          path = "tables/Table_descriptive_RCT_cci_1_ep_2025_10_10.docx")



