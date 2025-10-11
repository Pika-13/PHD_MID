#project on MID with Fariba
#Tidy RCT extracted table with pivot longer more rows to simplify summary table

#Date creation: 2025-10-07
#Last update: 2025-10-...

#Load necessary packages
lapply(c("tidyverse", "data.table","utils", "writexl", "readxl",
         "ggpubr", "scales", "flextable", "janitor", "labelled", "gtsummary", "dplyr"), require,
       character.only = TRUE)

# Assuming your dataset is called 
#included_from_compared_RCT = 142
#rows of protocol and publication (89)+ rows of protocols only (49) + publications only (4)
included_from_compared_RCT <- read_csv("database/csv_files_R_coding/Data_included_from_compared_RCT_2025_10_10.csv") 
#naming RCT_publications and 
RCT_publications <- included_from_compared_RCT %>% 
  filter(protocol == "Not a protocol")
write_csv(RCT_publications, "database/csv_files_R_coding/RCT_publications_2025_10_10.csv")

RCT_protocols_without_publication <- included_from_compared_RCT %>% 
  filter(protocol == "Protocol")
write_csv(RCT_protocols_without_publication, "database/csv_files_R_coding/RCT_protocols_without_publication_2025_10_10.csv")

#Pivot table for studies with both protocol and publication
RCT_matched_publications_protocols <- included_from_compared_RCT %>% 
  filter(protocol_published != "Neither protocol nor registry")
write_csv(RCT_matched_publications_protocols, "database/csv_files_R_coding/RCT_matched_publications_protocols_2025_10_10.csv")

#get database with only matched publication-protocol
# Step 1: Identify protocol-specific columns (ending with _protocol)
protocol_cols <- names(RCT_matched_publications_protocols)[grepl("_protocol$", names(RCT_matched_publications_protocols))]
# Step 2: Derive base column names by removing "_protocol"
base_cols <- gsub("_protocol$", "", protocol_cols)

# Step 3: Select other columns that are neither protocol_cols nor base_cols (shared columns)
shared_cols <- setdiff(names(RCT_matched_publications_protocols), c(protocol_cols, base_cols))

# Step 4: Prepare protocol dataframe (rename protocol columns to base names)
protocol_part <- RCT_matched_publications_protocols %>%
  select(all_of(c("trial_nr", shared_cols, protocol_cols))) %>%
  rename_with(~base_cols, all_of(protocol_cols)) %>%  # rename *_protocol columns to base names
  mutate(protocol = "protocol")

# Step 5: Prepare publication dataframe (use base columns + shared columns)
publication_part <- RCT_matched_publications_protocols %>%
  select(all_of(c("trial_nr", shared_cols, base_cols))) %>%
  mutate(protocol = "publication")

# Step 6: Combine protocol and publication parts
long_RCT_matched_publications_protocols <- bind_rows(protocol_part, publication_part) %>%
  relocate(protocol, .after = trial_nr) 

#save database only 89-89 publications and protocols
write_csv(long_RCT_matched_publications_protocols, "database/csv_files_R_coding/long_RCT_matched_publications_protocols_2025_10_10.csv") 

#ensure factors are factors
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
                        "cci_1st_ep", "cci_2nd_ep", "cci_expl_ep", 
                        "cd_cci_reported_in_results",
                        "cci_modification",
                        "primary_outcome_significant",
                        "statistically_significant_difference_for_cci",
                        "between_group_difference_of_cci_12",
                        "mean_or_median_used",
                        "significance_considered",
                        "conclusion_of_authors",
                        "study_type_mentioned", 
                        "type_of_study_sup_non_inf",
                        "ss_calculation", 
                        "cci_used_for_ss_calculation",
                        #"specification_of_ss_calculation",
                        "non_inf_margin",
                        #"definition_of_non_inf_margin",
                        "protocol_published"
                        )
long_RCT_matched_publications_protocols[, discrete_variables] <- lapply(
  long_RCT_matched_publications_protocols[, discrete_variables], factor)

#ensure all continuous variables are numeric
continuous_variables <- c("sample_size", "between_group_difference_of_cci")
long_RCT_matched_publications_protocols[, continuous_variables] <- lapply(
  long_RCT_matched_publications_protocols[, continuous_variables], as.numeric)

#ensure factor variables are ordered
long_RCT_matched_publications_protocols$protocol <- factor(long_RCT_matched_publications_protocols$protocol, 
                                      levels = c("protocol", "publication"), 
                                      labels = c("Protocol", "Publication"))
long_RCT_matched_publications_protocols$protocol_published <- factor(long_RCT_matched_publications_protocols$protocol_published, 
                                                levels = c("Protocol published", 
                                                           "Protocol unpublished",
                                                           "No protocol,registry"))
long_RCT_matched_publications_protocols$study_type_pilot <- factor(long_RCT_matched_publications_protocols$study_type_pilot, 
                                              levels = c("Pilot/ Safety study", "Not pilot study"))
long_RCT_matched_publications_protocols$pooled_analysis <- factor(long_RCT_matched_publications_protocols$pooled_analysis, 
                                             levels = c("Yes", "No"))
long_RCT_matched_publications_protocols$post_hoc_secondary_analysis <- factor(long_RCT_matched_publications_protocols$post_hoc_secondary_analysis, 
                                                         levels = c("Yes", "No"))
long_RCT_matched_publications_protocols$cci_ep <- factor(long_RCT_matched_publications_protocols$cci_ep, 
                                    levels = c("CCI primary endpoint", 
                                               "CCI non-primary endpoint",
                                               "CCI not an endpoint"))
long_RCT_matched_publications_protocols$cci_1st_ep <- factor(long_RCT_matched_publications_protocols$cci_1st_ep, 
                                    levels = c("Yes", "No"))
long_RCT_matched_publications_protocols$cci_2nd_ep <- factor(long_RCT_matched_publications_protocols$cci_2nd_ep, 
                                        levels = c("Yes", "No"))
long_RCT_matched_publications_protocols$cci_expl_ep <- factor(long_RCT_matched_publications_protocols$cci_expl_ep, 
                                        levels = c("Yes", "No"))
long_RCT_matched_publications_protocols$cd_cci_reported_in_results <- factor(long_RCT_matched_publications_protocols$cd_cci_reported_in_results, 
                                                        levels = c("Yes", "No"))
long_RCT_matched_publications_protocols$cci_modification <- factor(long_RCT_matched_publications_protocols$cci_modification, 
                                              levels = c("Yes", "No"))
long_RCT_matched_publications_protocols$primary_outcome_significant <- factor(long_RCT_matched_publications_protocols$primary_outcome_significant, 
                                                         levels = c("Yes", "No"))
long_RCT_matched_publications_protocols$statistically_significant_difference_for_cci <- factor(long_RCT_matched_publications_protocols$statistically_significant_difference_for_cci, 
                                                                          levels = c("Yes", "No"))
long_RCT_matched_publications_protocols$between_group_difference_of_cci_12 <- factor(long_RCT_matched_publications_protocols$between_group_difference_of_cci_12, 
                                                                levels = c("Yes", "No"))
long_RCT_matched_publications_protocols$mean_or_median_used <- factor(long_RCT_matched_publications_protocols$mean_or_median_used, 
                                                 levels = c("Mean", "Median"))
long_RCT_matched_publications_protocols$study_type_mentioned <- factor(long_RCT_matched_publications_protocols$study_type_mentioned, 
                                                  levels = c("Yes", "No"))
long_RCT_matched_publications_protocols$type_of_study_sup_non_inf <- factor(long_RCT_matched_publications_protocols$type_of_study_sup_non_inf, 
                                                       levels = c( "Superiority trial (explicited)",     "Superiority trial (assumed)", "Non-inferiority trial (explicited)", "Non-inferiority trial (assumed)",   
                                                                  "Equivalence trial", "Not mentioned"))       
long_RCT_matched_publications_protocols$ss_calculation <- factor(long_RCT_matched_publications_protocols$ss_calculation, 
                                            levels = c("Yes", "No"))
long_RCT_matched_publications_protocols$cci_used_for_ss_calculation <- factor(long_RCT_matched_publications_protocols$cci_used_for_ss_calculation, 
                                                         levels = c("Yes", "No", "Not mentioned"))
long_RCT_matched_publications_protocols$non_inf_margin <- factor(long_RCT_matched_publications_protocols$non_inf_margin, 
                                            levels = c("Yes", "No"))

long_RCT_matched_publications_protocols$significance_considered <- factor(
  long_RCT_matched_publications_protocols$significance_considered, levels= c("Statistical significance",
                                                                   "Both statistical and clinical significance"))
long_RCT_matched_publications_protocols$conclusion_of_authors <- factor(
  long_RCT_matched_publications_protocols$conclusion_of_authors,  
  levels =  c("Favorable to intervention","Neutral", "Not mentioned"," Unfavorable to intervention"))

#save
write_csv(long_RCT_matched_publications_protocols,"database/csv_files_R_coding/long_RCT_matched_publications_protocols_2025_10_10.csv")
write_xlsx(long_RCT_matched_publications_protocols,"database/csv_files_R_coding/long_RCT_matched_publications_protocols_2025_10_10.xlsx")

RCT_cci1ep <- NULL
#combine protocols without publication (49)
#and publications without protocol (4)
#and pivoted protocols with publication (89*2=178)
# = 231 total rows
long_231_RCT_protocols_and_or_trials <-
  rbind(long_RCT_matched_publications_protocols,
        RCT_protocols_without_publication %>% 
          select(names(long_RCT_matched_publications_protocols)) %>% 
          mutate(protocol = "protocol"),
        RCT_publications %>% 
          filter(protocol_published =="Neither protocol nor registry" | is.na(protocol_published)) %>% 
          select(names(long_RCT_matched_publications_protocols)) )
write_csv(long_231_RCT_protocols_and_or_trials, "database/csv_files_R_coding/long_231_RCT_protocols_and_or_trials_2025_10_10.csv")
#RCT with cci as 1 ep
RCT_cci_1_ep <- included_from_compared_RCT %>% filter(cci_1st_ep=="Yes" & protocol != "Protocol"&
                                                       cci_modification!="Yes")
write_csv(RCT_cci_1_ep, "database/csv_files_R_coding/RCT_cci_1_ep_2025_10_10.csv")
#write_xlsx(cci_1st_RCT, "database/csv_files_R_cod