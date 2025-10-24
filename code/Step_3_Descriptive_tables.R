#project on MID with Fariba
#Summary table with matches mismatches

#Date creation: 2025-10-07
#Last update: 2025-10-...

#Load necessary packages
lapply(c("tidyverse", "data.table","utils", "writexl", "readxl",
         "ggpubr", "scales", "flextable", "janitor", "labelled", "gtsummary","gt", "dplyr", "markdown"), require,
       character.only = TRUE)

# Open long_RCT_matched_publications_protocols
RCT_matched_publications_protocols <- read_csv("database/csv_files_R_coding/RCT_matched_publications_protocols_2025_10_12.csv") 
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
                        "significance_considered",
                        "mean_or_median_used",
                        "study_type_mentioned", 
                        "type_of_study_sup_non_inf",
                        "ss_calculation", 
                        "cci_used_for_ss_calculation",
                        #"specification_of_ss_calculation",
                        "non_inf_margin",
                        #"definition_of_non_inf_margin",
                        "cci_ep_protocol",
                        "ss_calculation_protocol", 
                        "cci_used_for_ss_calculation_protocol",
                        #"specification_of_ss_calculation",
                        "non_inf_margin_protocol",
                        #"definition_of_non_inf_margin",
                        "protocol_published")
RCT_matched_publications_protocols[, discrete_variables] <- lapply(
  RCT_matched_publications_protocols[, discrete_variables], factor)

#ensure all continuous variables are numeric
continuous_variables <- c("sample_size", "between_group_difference_of_cci")
RCT_matched_publications_protocols[, continuous_variables] <- lapply(
  RCT_matched_publications_protocols[, continuous_variables], as.numeric)



#ensure factor variables are ordered
RCT_matched_publications_protocols$continent <- factor(RCT_matched_publications_protocols$continent,
                       levels = names(sort(table(RCT_matched_publications_protocols$continent)
                                                       , decreasing = TRUE)))
RCT_matched_publications_protocols$discipline = factor(RCT_matched_publications_protocols$discipline, 
                                                       levels = names(sort(table(RCT_matched_publications_protocols$discipline), 
                                                                           descending=TRUE)))
 
RCT_matched_publications_protocols$protocol <- factor(RCT_matched_publications_protocols$protocol, 
                                                      levels = c("Protocol", "Publication"))
RCT_matched_publications_protocols$protocol_published <- factor(RCT_matched_publications_protocols$protocol_published, 
                                                                levels = c("Protocol published", 
                                                                           "Protocol unpublished",
                                                                           "No protocol,registry", "Neither protocol nor registry"))
RCT_matched_publications_protocols$study_type_pilot <- factor(RCT_matched_publications_protocols$study_type_pilot, 
                                                              levels = c("Pilot/ Safety study", "Not pilot study"))
RCT_matched_publications_protocols$pooled_analysis <- factor(RCT_matched_publications_protocols$pooled_analysis, 
                                                             levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
RCT_matched_publications_protocols$post_hoc_secondary_analysis <- factor(RCT_matched_publications_protocols$post_hoc_secondary_analysis, 
                                                                         levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
RCT_matched_publications_protocols$cci_ep <- factor(RCT_matched_publications_protocols$cci_ep, 
                                                    levels = c("CCI primary endpoint", 
                                                               "CCI non-primary endpoint"
                                                               # "CCI not an endpoint"
                                                    ))
RCT_matched_publications_protocols$cci_1st_ep = factor(RCT_matched_publications_protocols$cci_1st_ep,
                                                       levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
RCT_matched_publications_protocols$cci_2nd_ep = factor(RCT_matched_publications_protocols$cci_2nd_ep,
                                                       levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
RCT_matched_publications_protocols$cci_expl_ep = factor(RCT_matched_publications_protocols$cci_expl_ep,
                                                        levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
RCT_matched_publications_protocols$cd_cci_reported_in_results <- factor(RCT_matched_publications_protocols$cd_cci_reported_in_results, 
                                                                        levels = c("Yes", "No", "Not mentioned"),labels = c("Reported", "Not reported", "Not mentioned"))
RCT_matched_publications_protocols$cci_modification <- factor(RCT_matched_publications_protocols$cci_modification, 
                                                              levels = c("Yes", "No", "Not mentioned"),labels = c("Modified", "Not modified", "Not mentioned"))
RCT_matched_publications_protocols$primary_outcome_significant <- factor(RCT_matched_publications_protocols$primary_outcome_significant, 
                                                                         levels = c("Yes", "No"),labels = c("Significant", "Non-significant"))
RCT_matched_publications_protocols$statistically_significant_difference_for_cci <- factor(RCT_matched_publications_protocols$statistically_significant_difference_for_cci, 
                                                                                          levels = c("Yes", "No"),labels = c("Significant", "Non-significant"))
RCT_matched_publications_protocols$between_group_difference_of_cci_12 <- factor(RCT_matched_publications_protocols$between_group_difference_of_cci_12, 
                                                                                levels = c("Yes", "No"),labels = c("≥ 12 pts", "< 12 pts"))
RCT_matched_publications_protocols$mean_or_median_used <- factor(RCT_matched_publications_protocols$mean_or_median_used, 
                                                                 levels = c("Mean", "Median"))
RCT_matched_publications_protocols$study_type_mentioned <- factor(RCT_matched_publications_protocols$study_type_mentioned, 
                                                                  levels = c("Yes", "No"),labels = c("Mentioned", "Not mentioned"))
RCT_matched_publications_protocols$type_of_study_sup_non_inf <- factor(RCT_matched_publications_protocols$type_of_study_sup_non_inf,
                                                                       levels = c("Superiority trial",
                                                                                  "Non-inferiority trial","Equivalence trial", "Not mentioned"))

RCT_matched_publications_protocols$ss_calculation <- factor(RCT_matched_publications_protocols$ss_calculation, 
                                                            levels = c("Yes", "No"),labels = c("Yes", "No"))
RCT_matched_publications_protocols$cci_used_for_ss_calculation <- factor(RCT_matched_publications_protocols$cci_used_for_ss_calculation, 
                                                                         levels = c("Yes", "No"),labels = c("Used", "Not used"))
RCT_matched_publications_protocols$non_inf_margin <- factor(RCT_matched_publications_protocols$non_inf_margin, 
                                                            levels = c("Yes", "No"),labels = c("Defined", "Not defined"))
RCT_matched_publications_protocols$significance_considered <- factor(RCT_matched_publications_protocols$significance_considered, 
                                               levels = c("statistical significance", "both statistical and clinical significance"),
                                               labels = c("Statistical significance", "Both statistical and clinical significance"))
RCT_matched_publications_protocols$conclusion_of_authors <- factor(RCT_matched_publications_protocols$conclusion_of_authors, 
                                             levels = c("Favorable to intervention"))
RCT_matched_publications_protocols$cci_ep_protocol <- factor(RCT_matched_publications_protocols$cci_ep_protocol, 
                                                             levels = c("CCI primary endpoint", 
                                                                        "CCI non-primary endpoint",
                                                                        "CCI not an endpoint"
                                                             ))
RCT_matched_publications_protocols$study_type_mentioned_protocol <- factor(RCT_matched_publications_protocols$study_type_mentioned_protocol, 
                                                                           levels = c("Yes", "No") ,labels = c("Mentioned", "Not mentioned"))
RCT_matched_publications_protocols$type_of_study_sup_non_inf_protocol <- factor(RCT_matched_publications_protocols$type_of_study_sup_non_inf_protocol,
                                                                                levels = c("Superiority trial",
                                                                                           "Non-inferiority trial","Equivalence trial", "Not mentioned"))

RCT_matched_publications_protocols$ss_calculation_protocol <- factor(RCT_matched_publications_protocols$ss_calculation_protocol, 
                                                                     levels = c("Yes", "No") ,labels = c("Calculated", "Not calculated"))
RCT_matched_publications_protocols$cci_used_for_ss_calculation_protocol <- factor(RCT_matched_publications_protocols$cci_used_for_ss_calculation_protocol, 
                                                                                  levels = c("Yes", "No"),labels = c("Used", "Not used"))
RCT_matched_publications_protocols$non_inf_margin_protocol <- factor(RCT_matched_publications_protocols$non_inf_margin_protocol, 
                                                                     levels = c("Yes", "No"),labels = c("Defined", "Not defined"))

RCT_matched_publications_protocols %>% 
  filter(ss_calculation=="Yes") %>%
  select(protocol_published, ss_calculation_protocol, ss_calculation, cci_used_for_ss_calculation_protocol,cci_used_for_ss_calculation ) %>% 
  group_by(ss_calculation_protocol, cci_used_for_ss_calculation_protocol, ss_calculation, cci_used_for_ss_calculation, protocol_published) %>%
  summarize(n=n())
#-----------------------------------
#Mismatches
#open table with _protocol variables
mismatch_cci_table <- 
  RCT_matched_publications_protocols %>%
  select(cci_ep,cci_ep_protocol) %>% 
  tbl_summary(
    by = cci_ep ,
    label = cci_ep_protocol ~"CCI(R) as an endpoint in pre-trial documentation",
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
    values = c("", "CCI(R) as an endpoint in published trial"),# Adjust number of columns
    colwidths = c(1, 2)                       # Set number of columns under each spanner
  ) %>% 
  bold(part = "header", i=1) %>% 
  bold(part="body", j=1) %>%
  add_footer_lines(values = "* Studies with CCI(R) designation as non-primary endpoint consist of studies with CCI(R) designation as either secondary or exploratory endpoint."
  )%>%
  add_footer_lines(
    values = "Abbreviations: CCI(R), Comprehesive Complication Index; RCT, Randomized Controlled Trial."
  ) %>% 
   set_caption("Table 1. Concordance Analysis Between Pre-trial Documentation and Published Trials for CCI® Designation as an Endpoint")

  save_as_docx(mismatch_cci_table, path = "tables/mismatch_table_cci.docx")

  #-----------------------------------------------------
  #Mismatches
  #open table with _protocol variables
  RCT_matched_publications_protocols <- read_csv("database/csv_files_R_coding/RCT_matched_publications_protocols_2025_10_12.csv") 
  RCT_matched_publications_protocols$study_type_mentioned <- factor(RCT_matched_publications_protocols$study_type_mentioned  , 
                                                                           levels = c("Yes", "No"
                                                                           ), labels = c("Mentioned", "Not mentioned"))
  RCT_matched_publications_protocols$study_type_mentioned_protocol <- factor(RCT_matched_publications_protocols$study_type_mentioned_protocol, 
                                                                                    levels = c("Yes", "No"),labels = c("Mentioned", "Not mentioned"))
  
  mismatch_table_framework <- 
    RCT_matched_publications_protocols %>%
    #filter(study_type_mentioned_protocol=="Yes")
    select(study_type_mentioned_protocol,study_type_mentioned) %>% 
    tbl_summary(
      by = study_type_mentioned ,
      label =  study_type_mentioned_protocol ~"Study framework explicited in pre-trial documentation",
      statistic = list(
        study_type_mentioned_protocol ~ "{p}% ({n}/{N_nonmiss})"),
      digits = list(
        all_categorical() ~ c(1,0,0)
        # all_continuous() ~ c(2,2,2)# 0 decimals for counts, 1 for percentages
      ), missing = "no"
    ) %>% 
    # add_overall(last=TRUE) %>% 
    as_flex_table()      %>% 
    add_header_row(
      values = c("", "Study framework explicited in published trial"),# Adjust number of columns
      colwidths = c(1, 2)                       # Set number of columns under each spanner
    ) %>% 
    bold(part = "header", i=1) %>% 
    bold(part="body", j=1) %>%
    set_caption("Table 2. Concordance Analysis Between Pre-trial Documentation and Published Trials for Study Framework Explicitation")
  
  save_as_docx(mismatch_table_framework, path = "tables/mismatch_table_study_framework.docx")
  
  #-----------------------------------------------------
  #Mismatches
  #open table with _protocol variables
  RCT_matched_publications_protocols <- read_csv("database/csv_files_R_coding/RCT_matched_publications_protocols_2025_10_12.csv") 
  RCT_matched_publications_protocols$cci_used_for_ss_calculation <- factor(RCT_matched_publications_protocols$cci_used_for_ss_calculation  , 
                                                      levels = c("Yes", "No"
                                                      ), labels = c("Used", "Not used"))
  RCT_matched_publications_protocols$cci_used_for_ss_calculation_protocol <- factor(RCT_matched_publications_protocols$cci_used_for_ss_calculation_protocol, 
                                                               levels = c("Yes", "No"),labels = c("Used", "Not used"))
 
   mismatch_table_cci_ss <- 
    RCT_matched_publications_protocols %>%
   # filter(ss_calculation_protocol=="Yes") %>% 
    select(cci_used_for_ss_calculation_protocol,cci_used_for_ss_calculation) %>% 
    tbl_summary(
      by = cci_used_for_ss_calculation,
      label = c(
        cci_used_for_ss_calculation_protocol ~"CCI usage for sample size calculation in pre-trial documentation"),
      statistic = list(
        cci_used_for_ss_calculation_protocol ~ "{p}% ({n}/{N})"),
      digits = list(
        all_categorical() ~ c(1,0,0)
        # all_continuous() ~ c(2,2,2)# 0 decimals for counts, 1 for percentages
      ), missing = "no"
    ) %>% 
    # add_overall(last=TRUE) %>% 
    as_flex_table()      %>% 
    add_header_row(
      values = c("", "CCI usage for sample size calculation in published trial"),# Adjust number of columns
      colwidths = c(1, 2)                       # Set number of columns under each spanner
    ) %>% 
    bold(part = "header", i=1) %>% 
    bold(part = "body", j=1) %>% 
     set_caption("Table 3. Concordance Analysis Between Pre-trial Documentation and Published Trials for CCI usage for sample size calculation")
   
  
  save_as_docx(mismatch_table_cci_ss, path = "tables/mismatch_table_cci_for_ss.docx")
  
  
  
  
  
  
  
  
#-------------------------------------------------

#List with all renamed variables
var_list_total <- list(
 # paper_type ~"Paper type", 
  # protocol ~"Protocol", 
  #protocol_published_original_columnn~"Protocol publication status", 
  #study_type_pilot ~"Pilot/ feasibility study",
 # pooled_analysis ~"pooled_analysis",
 # post_hoc_secondary_analysis ~"Post-hoc secondary analysis",
  continent ~"Continent", 
 discipline ~"Discipline",
 # sample_size ~ "Sample size",
  cci_ep ~"CCI use as endpoint in published trial",
 statistically_significant_difference_for_cci ~"CCI difference significant",
 between_group_difference_of_cci_12~"CCI difference ≥ 12 pts",
 significance_considered ~"Significance considered",
# conclusion_of_authors ~"Conclusion favours intervention",
# between_group_difference_of_cci ~"CCI difference [pts]",
 type_of_study_sup_non_inf ~"Trial aim explicited in published trial",
# ss_calculation ~"Sample size calculation in published trial",
 cci_used_for_ss_calculation ~ "CCI used for sample size calculation in published trial",
 #cd_cci_reported_in_results ~ "CCI report in published trial",
  #cci_modification ~"CCI modification",
  #specification_of_modification ~"CCI modification specification",
 protocol_published ~"Pre-trial documentation type"
  #primary_outcome_significant ~ "Primary outcome significant",
  # mean_or_median_used ~ "Mean or median used",
    #specification_of_ss_calculation ~"Details on sample size calculation",
 # non_inf_margin~"Non-inferiority margin definition in published trial",
  #definition_of_non_inf_margin ~"Details on non-inferiority margin",
 # cci_ep_protocol ~"CCI use as endpoint in pre-trial documentation",
 # type_of_study_sup_non_inf_protocol ~"Trial framework explicited in pre-trial documentation",
 # ss_calculation_protocol~"Sample size calculation in pre-trial documentation"
 # cci_used_for_ss_calculation_protocol~ "CCI used for sample size calculation in pre-trial documentation",
 # non_inf_margin_protocol~"Non-inferiority margin definition in pre-trial documentation"
  )

  RCT_matched_publications_protocols <- read_csv("database/csv_files_R_coding/RCT_matched_publications_protocols_2025_10_12.csv") 
  
  RCT_matched_publications_protocols$continent <- factor(RCT_matched_publications_protocols$continent,
                                                         levels = names(sort(table(RCT_matched_publications_protocols$continent)
                                                                             , decreasing = TRUE)))
  RCT_matched_publications_protocols$discipline = factor(RCT_matched_publications_protocols$discipline, 
                                                         levels = names(sort(table(RCT_matched_publications_protocols$discipline), 
                                                                             descending=TRUE)))
  
  RCT_matched_publications_protocols$protocol <- factor(RCT_matched_publications_protocols$protocol, 
                                                        levels = c("Protocol", "Publication"))
  RCT_matched_publications_protocols$protocol_published <- factor(RCT_matched_publications_protocols$protocol_published, 
                                                                  levels = c("Protocol published", 
                                                                             "Protocol unpublished",
                                                                             "No protocol,registry", "Neither protocol nor registry"))
  RCT_matched_publications_protocols$study_type_pilot <- factor(RCT_matched_publications_protocols$study_type_pilot, 
                                                                levels = c("Pilot/ Safety study", "Not pilot study"))
  RCT_matched_publications_protocols$pooled_analysis <- factor(RCT_matched_publications_protocols$pooled_analysis, 
                                                               levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
  RCT_matched_publications_protocols$post_hoc_secondary_analysis <- factor(RCT_matched_publications_protocols$post_hoc_secondary_analysis, 
                                                                           levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
  RCT_matched_publications_protocols$cci_ep <- factor(RCT_matched_publications_protocols$cci_ep, 
                                                      levels = c("CCI primary endpoint", 
                                                                 "CCI non-primary endpoint"
                                                                 # "CCI not an endpoint"
                                                      ))
  RCT_matched_publications_protocols$cci_1st_ep = factor(RCT_matched_publications_protocols$cci_1st_ep,
                                                         levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
  RCT_matched_publications_protocols$cci_2nd_ep = factor(RCT_matched_publications_protocols$cci_2nd_ep,
                                                         levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
  RCT_matched_publications_protocols$cci_expl_ep = factor(RCT_matched_publications_protocols$cci_expl_ep,
                                                          levels = c("Yes", "No", "Not mentioned"),labels = c("Yes", "No", "Not mentioned"))
  RCT_matched_publications_protocols$cd_cci_reported_in_results <- factor(RCT_matched_publications_protocols$cd_cci_reported_in_results, 
                                                                          levels = c("Yes", "No", "Not mentioned"),labels = c("Reported", "Not reported", "Not mentioned"))
  RCT_matched_publications_protocols$cci_modification <- factor(RCT_matched_publications_protocols$cci_modification, 
                                                                levels = c("Yes", "No", "Not mentioned"),labels = c("Modified", "Not modified", "Not mentioned"))
  RCT_matched_publications_protocols$primary_outcome_significant <- factor(RCT_matched_publications_protocols$primary_outcome_significant, 
                                                                           levels = c("Yes", "No"),labels = c("Significant", "Non-significant"))
  RCT_matched_publications_protocols$statistically_significant_difference_for_cci <- factor(RCT_matched_publications_protocols$statistically_significant_difference_for_cci, 
                                                                                            levels = c("Yes", "No"),labels = c("Significant", "Non-significant"))
  RCT_matched_publications_protocols$between_group_difference_of_cci_12 <- factor(RCT_matched_publications_protocols$between_group_difference_of_cci_12, 
                                                                                  levels = c("Yes", "No"),labels = c("≥ 12 pts", "< 12 pts"))
  RCT_matched_publications_protocols$mean_or_median_used <- factor(RCT_matched_publications_protocols$mean_or_median_used, 
                                                                   levels = c("Mean", "Median"))
  RCT_matched_publications_protocols$study_type_mentioned <- factor(RCT_matched_publications_protocols$study_type_mentioned, 
                                                                    levels = c("Yes", "No"),labels = c("Mentioned", "Not mentioned"))
  RCT_matched_publications_protocols$type_of_study_sup_non_inf <- factor(RCT_matched_publications_protocols$type_of_study_sup_non_inf,
                                                                         levels = c("Superiority trial",
                                                                                    "Non-inferiority trial","Equivalence trial", "Not mentioned"))
  
  RCT_matched_publications_protocols$ss_calculation <- factor(RCT_matched_publications_protocols$ss_calculation, 
                                                              levels = c("Yes", "No"),labels = c("Yes", "No"))
  RCT_matched_publications_protocols$cci_used_for_ss_calculation <- factor(RCT_matched_publications_protocols$cci_used_for_ss_calculation, 
                                                                           levels = c("Yes", "No"),labels = c("Used", "Not used"))
  RCT_matched_publications_protocols$non_inf_margin <- factor(RCT_matched_publications_protocols$non_inf_margin, 
                                                              levels = c("Yes", "No"),labels = c("Defined", "Not defined"))
  RCT_matched_publications_protocols$significance_considered <- factor(RCT_matched_publications_protocols$significance_considered, 
                                                                       levels = c("Statistical significance", "Both statistical and clinical significance"))
  RCT_matched_publications_protocols$conclusion_of_authors <- factor(RCT_matched_publications_protocols$conclusion_of_authors, 
                                                                     levels = c("Favorable to intervention"))
  RCT_matched_publications_protocols$cci_ep_protocol <- factor(RCT_matched_publications_protocols$cci_ep_protocol, 
                                                               levels = c("CCI primary endpoint", 
                                                                          "CCI non-primary endpoint",
                                                                          "CCI not an endpoint"
                                                               ))
  RCT_matched_publications_protocols$study_type_mentioned_protocol <- factor(RCT_matched_publications_protocols$study_type_mentioned_protocol, 
                                                                             levels = c("Yes", "No") ,labels = c("Mentioned", "Not mentioned"))
  RCT_matched_publications_protocols$type_of_study_sup_non_inf_protocol <- factor(RCT_matched_publications_protocols$type_of_study_sup_non_inf_protocol,
                                                                                  levels = c("Superiority trial",
                                                                                             "Non-inferiority trial","Equivalence trial", "Not mentioned"))
  
  RCT_matched_publications_protocols$ss_calculation_protocol <- factor(RCT_matched_publications_protocols$ss_calculation_protocol, 
                                                                       levels = c("Yes", "No") ,labels = c("Calculated", "Not calculated"))
  RCT_matched_publications_protocols$cci_used_for_ss_calculation_protocol <- factor(RCT_matched_publications_protocols$cci_used_for_ss_calculation_protocol, 
                                                                                    levels = c("Yes", "No"),labels = c("Used", "Not used"))
  RCT_matched_publications_protocols$non_inf_margin_protocol <- factor(RCT_matched_publications_protocols$non_inf_margin_protocol, 
                                                                       levels = c("Yes", "No"),labels = c("Defined", "Not defined"))
  
  

#table code
Table_descriptive_RCT_matched_publications_protocols <- RCT_matched_publications_protocols %>% 
  select(#protocol, 
    #study_type_pilot ,pooled_analysis,
       #  post_hoc_secondary_analysis,
         continent, 
    discipline, 
         #sample_size,
         cci_ep ,
        # primary_outcome_significant ,
         statistically_significant_difference_for_cci ,
         between_group_difference_of_cci_12,
    significance_considered,
   # conclusion_of_authors,
        # between_group_difference_of_cci,
        # mean_or_median_used,
       #  study_type_mentioned,
         type_of_study_sup_non_inf,
       #  ss_calculation,
         cci_used_for_ss_calculation,
    protocol_published ,
       # non_inf_margin,
    #cci_ep_protocol,
  # type_of_study_sup_non_inf_protocol
   # ss_calculation_protocol,
   # cci_used_for_ss_calculation_protocol,
   # non_inf_margin_protocol 
   ) %>% 
  tbl_summary(
    #by = protocol,
    label = var_list_total,
    statistic = list(
    #  c("between_group_difference_of_cci",
    #    "sample_size") ~ "{median} ({p25}-{p75}), ({N_nonmiss})",
      c(#"protocol", 
        "protocol_published" ,
        #"study_type_pilot" ,"pooled_analysis",
        # "post_hoc_secondary_analysis",
        "continent", "discipline",
      "cci_ep" ,
       # "primary_outcome_significant" ,
         "statistically_significant_difference_for_cci" ,
         "between_group_difference_of_cci_12",
      "significance_considered",
     # "conclusion_of_authors",
       #  "mean_or_median_used",
         "type_of_study_sup_non_inf",
       #  "ss_calculation",
         "cci_used_for_ss_calculation"
      #"non_inf_margin",
       # "cci_ep_protocol",
      #  "type_of_study_sup_non_inf_protocol",
      #  "ss_calculation_protocol",
      #  "cci_used_for_ss_calculation_protocol",
      #  "non_inf_margin_protocol"
      )  ~ "{p}% ({n}/{N_nonmiss})"),
    missing = "no" ,
    digits = list(
      all_categorical() ~ c(1,0,0),
      all_continuous() ~ c(0,0,0)# 0 decimals for counts, 1 for percentages
     ))%>%
  as_flex_table()      %>% 
  add_footer_lines(values = "Continuous variables are expressed in median (interquartile), discrete variables are expressed in percentage (number of observations/ total non-missing)
                     ")%>%
    add_footer_lines(values = "* Studies with CCI(R) designation as non-primary endpoint consist of studies with CCI(R) designation as either secondary or exploratory endpoint."
                     )%>%
  add_footer_lines(values = "Abbreviations: CCI, Comprehensive Complication Index; ICU, Intensive Care Unit; RCT, Randomized Controlled Trial."
                   ) %>% 
  set_caption("Table X. Characteristics of published RCTs with an available Pre-trial Documentation")



  #add_overall()
  #add_p() %>%
#Save Table_descriptive
save_as_docx(Table_descriptive_RCT_matched_publications_protocols, path = "tables/Table_descriptive_RCT_matched_publications_protocols.docx")


























#-----------------------------------
#table with cci = 1st ep

RCT_cci_1_ep <- read_csv("database/csv_files_R_coding/RCT_cci_1_ep_2025_10_12.csv")
#List with all renamed variables
var_list_cci_1_ep <- list(
  # paper_type ~"Paper type", 
  #protocol ~"Protocol", 
  protocol_published ~"Protocol publication",
  #protocol_published_original_columnn~"Protocol publication status", 
  # study_type_pilot ~"Pilot/ feasibility study",
  # pooled_analysis ~"pooled_analysis",
  # post_hoc_secondary_analysis ~"Post-hoc secondary analysis",
  # discipline ~"Discipline",
  # continent ~"Continent", 
  sample_size ~ "Sample size",
  cci_ep ~"CCI use as endpoint in paper",
  # cd_cci_reported_in_results ~ "CCI report in results",
  #  cci_modification ~"CCI modification",
  #specification_of_modification ~"CCI modification specification",
  primary_outcome_significant ~ "Primary outcome significant",
  statistically_significant_difference_for_cci ~"CCI difference significant",
  significance_considered ~"Significance considered",
  conclusion_of_authors ~"Conclusion favours intervention",
  between_group_difference_of_cci_12~"CCI difference ≥ 12 pts",
  between_group_difference_of_cci ~"CCI difference [pts]",
  mean_or_median_used ~ "Mean or median used",
  type_of_study_sup_non_inf ~"Trial type (superiority/ non-inferiority) in pre-trial documentation",
  ss_calculation ~"Sample size calculation",
  cci_used_for_ss_calculation ~ "CCI used for sample size calculation",
  #specification_of_ss_calculation ~"Details on sample size calculation",
  non_inf_margin~"Non-inferiority margin definition",
  #definition_of_non_inf_margin ~"Details on non-inferiority margin",
  protocol_published ~"Protocol publication status",
  significance_considered ~"Significance considered",
  conclusion_of_authors ~"Conclusion favours intervention",
  cci_ep_protocol ~"CCI use as endpoint in pre-trial documentation",
  type_of_study_sup_non_inf_protocol ~"Trial type (superiority/ non-inferiority) in pre-trial documentation",
#  ss_calculation_protocol ~"Sample size calculation in pre-trial documentation",
  cci_used_for_ss_calculation_protocol ~ "CCI used for sample size calculation in pre-trial documentation",
  #specification_of_ss_calculation ~"Details on sample size calculation",
  non_inf_margin_protocol~"Non-inferiority margin definition in pre-trial documentation"
  #definition_of_non_inf_margin ~"Details on non-inferiority margin",
)


#table code
Table_descriptive_RCT_cci_1_ep <- RCT_matched_publications_protocols %>% 
  filter(cci_1st_ep=="Yes") %>% 
  select(protocol_published ,
         post_hoc_secondary_analysis,
         sample_size,
         cci_ep ,cci_modification,
         primary_outcome_significant,
         statistically_significant_difference_for_cci,
         significance_considered ,
         conclusion_of_authors,
         between_group_difference_of_cci_12,
         between_group_difference_of_cci,
         mean_or_median_used ,
         type_of_study_sup_non_inf,
         ss_calculation,
         cci_used_for_ss_calculation,
         non_inf_margin,
         protocol_published ,
         significance_considered ,
         conclusion_of_authors ,
         cci_ep_protocol ,
         type_of_study_sup_non_inf_protocol,
         ss_calculation_protocol,
         cci_used_for_ss_calculation_protocol,
         non_inf_margin_protocol
  ) %>% 
  tbl_summary(
    #by = protocol,
    label = var_list_cci_1_ep,
    statistic = list(
      c("between_group_difference_of_cci",
        "sample_size") ~ "{median} ({p25}-{p75}), ({N_nonmiss})",
      c("protocol_published" ,
        "post_hoc_secondary_analysis","cci_ep" ,"cci_modification",
        "primary_outcome_significant",
        "statistically_significant_difference_for_cci",
        "significance_considered" ,
        "conclusion_of_authors",
        "between_group_difference_of_cci_12", "mean_or_median_used" ,
        "type_of_study_sup_non_inf",
        "ss_calculation",
        "cci_used_for_ss_calculation",
        "non_inf_margin",
        "protocol_published" ,
        "significance_considered" ,
        "conclusion_of_authors" ,
        "cci_ep_protocol" ,
        "type_of_study_sup_non_inf_protocol",
        "ss_calculation_protocol",
        "cci_used_for_ss_calculation_protocol",
        "non_inf_margin_protocol"
      )  ~ "{p_nonmiss}% ({n}/{N_nonmiss})"),
    digits = list(
      all_categorical() ~ c(1,0,0),
      all_continuous() ~ c(0,0,0),# 0 decimals for counts, 1 for percentages
      missing = "no" )) 
#add_overall()
#add_p() %>%
#Save Table_descriptive
Table_descriptive_RCT_cci_1_ep_path<-paste(getwd(), "/tables/Table_descriptive_RCT_cci_1_ep.docx", sep = "")
Table_descriptive_RCT_cci_1_ep%>%as_flex_table()%>%
  flextable::save_as_docx(Table_descriptive_RCT_cci_1_ep, 
                          path = "tables/Table_descriptive_RCT_cci_1_ep.docx")

plot_cci_ep_pre_trial <- 
  RCT_matched_publications_protocols %>% 
  select(protocol_published, between_group_difference_of_cci, 
         statistically_significant_difference_for_cci,
         significance_considered, conclusion_of_authors) %>% 
  tbl_summary(
    by = protocol_published,
    label = c(between_group_difference_of_cci~ "CCI difference" ,
              statistically_significant_difference_for_cci ~ "CCI difference significant"),
    statistic = list(
      between_group_difference_of_cci~ "{mean} ({N_nonmiss})",
      statistically_significant_difference_for_cci ~ "{p}% ({n}/{N_nonmiss})"),
    digits = list(
      all_categorical() ~ c(1,0,0),
      all_continuous() ~ c(2,2,2)# 0 decimals for counts, 1 for percentages
    ), missing = "no"
  ) %>%
  add_ci()


#List RCTs variables
var_list_long_RCT_matched_publications_protocols <- list(
  # paper_type ~"Paper type", 
  # protocol ~"Protocol", 
  # protocol_published ~"Protocol publication status",
  #protocol_published_original_columnn~"Protocol publication status", 
  # study_type_pilot ~"Pilot/ feasibility study",
  #pooled_analysis ~"pooled_analysis",
  # post_hoc_secondary_analysis ~"Post-hoc secondary analysis",
  # discipline ~"Discipline",
  # continent ~"Continent", 
  # sample_size ~ "Sample size",
  cci_ep ~"CCI use as endpoint",
  # cd_cci_reported_in_results ~ "CCI report in results",
  # cci_modification ~"CCI modification",
  #specification_of_modification ~"CCI modification specification",
  # primary_outcome_significant ~ "Primary outcome significant",
  # statistically_significant_difference_for_cci ~"CCI difference significant",
  # between_group_difference_of_cci_12~"CCI difference ≥ 12 pts",
  # between_group_difference_of_cci ~"CCI difference [pts]",
  # mean_or_median_used ~ "Mean or median used",
  # study_type_mentioned ~"Trial type mentioned",
  type_of_study_sup_non_inf ~"Trial type (superiority/ non-inferiority)", 
  ss_calculation ~"Sample size calculation",
  cci_used_for_ss_calculation ~ "CCI used for sample size calculation",
  #specification_of_ss_calculation ~"Details on sample size calculation",
  non_inf_margin~"Non-inferiority margin definition"
  #definition_of_non_inf_margin ~"Details on non-inferiority margin",
  #protocol_published ~"Protocol publication status")
)

#table code
Table_descriptive_long_RCT_matched_publications_protocols <- long_RCT_matched_publications_protocols %>%
  select(protocol, cci_ep, 
         #study_type_mentioned,
         type_of_study_sup_non_inf,ss_calculation,
         cci_used_for_ss_calculation,non_inf_margin) %>% 
  tbl_summary(
    by = protocol,
    label = var_list_long_RCT_matched_publications_protocols,
    statistic = list(
      # c("sample_size", "between_group_difference_of_cci") ~ "{median} ({p25}-{p75})",
      c("cci_ep",
        #"study_type_mentioned",
        "type_of_study_sup_non_inf",
        "ss_calculation", "cci_used_for_ss_calculation",
        "non_inf_margin") ~ "{p}% ({n}/{N_nonmiss})"),
    digits = list(
      all_categorical() ~ c(1,0,0),
      all_continuous() ~ c(2,2,2)# 0 decimals for counts, 1 for percentages
    ), missing = "no"
  )  
#add_overall()
#add_p() %>%

#Save Table_descriptive
Table_descriptive_long_RCT_matched_publications_protocols_path<-paste(getwd(), "/tables/Table_descriptive_long_RCT_matched_publications_protocols.docx", sep = "")
Table_descriptive_long_RCT_matched_publications_protocols%>%as_flex_table()%>%
  flextable::save_as_docx(Table_descriptive_long_RCT_matched_publications_protocols, 
                          path = "tables/Table_descriptive_long_RCT_matched_publications_protocols.docx")



#table code
Table_descriptive_long_RCT_matched_publications_protocols <- RCT_matched_publications_protocols %>% 
  filter(protocol=="Publication") %>% 
  select(#protocol, 
    protocol_published ,
    #study_type_pilot ,pooled_analysis,
    #  post_hoc_secondary_analysis,
    continent, discipline, 
    # sample_size,
    cci_ep ,
    primary_outcome_significant ,
    statistically_significant_difference_for_cci ,
    between_group_difference_of_cci_12,
    between_group_difference_of_cci,
    mean_or_median_used,
    #  study_type_mentioned,
    type_of_study_sup_non_inf,
    ss_calculation,
    cci_used_for_ss_calculation,
    non_inf_margin,
    cci_ep_protocol,
    type_of_study_sup_non_inf_protocol,
    ss_calculation_protocol,
    cci_used_for_ss_calculation_protocol,
    non_inf_margin_protocol ) %>% 
  tbl_summary(
    #by = protocol,
    label = var_list_total,
    statistic = list(
      c("between_group_difference_of_cci",
        "sample_size") ~ "{median} ({p25}-{p75}), ({N_obs})",
      c(#"protocol", 
        "protocol_published" ,"study_type_pilot" ,"pooled_analysis",
        "post_hoc_secondary_analysis","cci_ep" ,
        "primary_outcome_significant" ,
        "statistically_significant_difference_for_cci" ,
        "between_group_difference_of_cci_12",
        "mean_or_median_used",
        "type_of_study_sup_non_inf",
        "ss_calculation",
        "cci_used_for_ss_calculation","non_inf_margin",
        "cci_ep_protocol",
        "type_of_study_sup_non_inf_protocol",
        "ss_calculation_protocol",
        "cci_used_for_ss_calculation_protocol",
        "non_inf_margin_protocol")  ~ "{p}% ({n}/{N_obs})"),
    missing = "ifany" ,
    digits = list(
      all_categorical() ~ c(1,0,0),
      all_continuous() ~ c(0,0,0)# 0 decimals for counts, 1 for percentages
    )) 
#add_overall()
#add_p() %>%
#Save Table_descriptive
Table_descriptive_long_RCT_matched_publications_protocols_path<-paste(getwd(), "/tables/Table_descriptive_long_RCT_matched_publications_protocols.docx", sep = "")
Table_descriptive_long_RCT_matched_publications_protocols%>%as_flex_table()%>%
  flextable::save_as_docx(Table_descriptive_long_RCT_matched_publications_protocols, 
                          path = "tables/Table_descriptive_long_RCT_matched_publications_protocols.docx")





RCT_matched_publications_protocols %>% 
  filter(ss_calculation=="Yes") %>%
  select(protocol_published, ss_calculation_protocol, ss_calculation, cci_used_for_ss_calculation_protocol,cci_used_for_ss_calculation ) %>% 
  group_by(ss_calculation_protocol, cci_used_for_ss_calculation_protocol, ss_calculation, cci_used_for_ss_calculation, protocol_published) %>%
  summarize(n=n())

#further analyses
#number 1:
#see slide 31 course plot registered vs protocol vs unpublished protocol -> CCI difference/ CCI significant


