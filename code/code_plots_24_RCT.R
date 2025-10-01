#project on MID with Fariba
#23 RCT analysis
#Load necessary packages
lapply(c("tidyverse", "data.table", "dplyr","utils", "writexl", "readxl",
         "ggpubr", "scales", "flextable", "janitor", "labelled"), require,
       character.only = TRUE)
#---------------------
#reading 23 RCT excel file, cleaning, tidying
extracted_23_RCT <- janitor::clean_names(read_xlsx("Data_23_RCT_Joana_2025_09_17.xlsx",
                              col_names = TRUE)[-1, ])%>% 
  mutate(
    article_id = as.character(article_id),
    title =  as.character(title),
    publication_year = as.integer(publication_year),
    vol_nr=as.integer(vol_nr),                                                      
    issue_nr = as.integer(issue_nr),
    journal=as.character(journal),
    paper_type = factor(case_when(
      str_detect(misc_2, "Article") ~ "Article",
      str_detect(misc_2, "Conference Paper") ~ "Conference Paper",
      .default = "Unknown"), levels = c("Unknown", "Article", "Conference Paper")),
    web_url = as.character(web_url),
    doi = as.character(doi),
    trial_nr= as.character(trial_nr), 
    protocol= factor(protocol, levels = c("na", "0", "1"), labels = c("Unknown","Not a protocol", "Protocol")),
    protocol_published_original_column = factor(case_when(
      protocol=="Protocol" &
        results_of_the_protocol_already_published =="1" & 
        published_results_included_in_the_list == "1" ~ "Study published, listed",
      results_of_the_protocol_already_published =="1" &
        protocol=="Protocol" & 
        published_results_included_in_the_list == "0" ~ "Study published, not listed",
      protocol=="Protocol" & 
        results_of_the_protocol_already_published =="0" ~ "Study not published yet",
      protocol=="Not a protocol" ~ NA),
      levels = c("Study published, listed", 
                 "Study published, not listed", 
                 "Study not published yet")),
    study_type = factor(case_when(
      pilot_feasibility == "0" & type_of_study == "0" ~ "Superiority study",
      pilot_feasibility == "0" & type_of_study == "1" ~ "Non-inferiority study",
      pilot_feasibility == "1" ~ "Pilot/ Safety study"), 
      levels = c("Superiority study", "Non-inferiority study", "Pilot/ Safety study")),
    pooled_analysis = factor(pooled_analysis, 
                             levels = c("0", "1"), 
                             labels = c("No", "Yes")),
    post_hoc_secondary_analysis = factor(post_hoc_secondary_analysis,
                                         levels = c("0", "1"),
                                         labels = c("No", "Yes")),
    discipline_cleaned = ifelse(
      discipine %in% as.character(1:13),
      discipine,
      "13"
    ),
    discipline = factor(discipline_cleaned, levels = c("1", "2", "3", "4", "5", "6", 
                                              "7", "8", "9", "10", "11", 
                                              "12", "13"),
                       labels = c("Abdominal Surgery", 
                       "Transplant", "Urology", "Orthopedics", 
                       "Gynecology", "Emergency","Thoracic","Vascular", 
                       "ICU,Anesthesiology","Radiology", "Gastroenterology",
                       "Angiology", "Other disciplines with invasive procedures")),
    continent = factor(continent_last_author, levels = c("1", "2", "3", "4", "5", "6"),
                       labels = c("Europe", "North America", "South America", 
                                  "Asia", "Australia/ New Zealand","Africa")),
    sample_size = as.numeric(sample_size),
    cci_1st_ep = factor(cci_1st_ep,
                        levels = c("0", "1"),
                        labels = c("No", "Yes")),
    cci_2nd_ep = factor(cci_2nd_ep,
                        levels = c("0", "1"),
                        labels = c("No", "Yes")),
    cci_expl_ep = factor(cci_expl_ep,
                        levels = c("0", "1"),
                        labels = c("No", "Yes")),
    cci_report_in_results = factor(cci_reported_in_results,
                        levels = c("0", "1"),
                        labels = c("No", "Yes")),
    cci_ep = factor(case_when(cci_1st_ep == "Yes" & cci_2nd_ep == "No" ~ "CCI primary endpoint",
                    cci_1st_ep == "No" & cci_2nd_ep == "Yes" ~ "CCI secondary endpoint",
                    cci_1st_ep == "Yes" & cci_2nd_ep == "Yes" ~ "CCI both primary and secondary endpoint",
                    cci_expl_ep == "Yes" ~ "CCI exploratory endpoint"),
                    levels = c("CCI primary endpoint", "CCI secondary endpoint",
                               "CCI both primary and secondary endpoint",
                               "CCI exploratory endpoint")),
    #  cci_modification = factor(cci_modification,
  #                      levels = c("0", "1"),
  #                      labels = c("No", "Yes")),
   # specification_of_modification = as.character(specification_of_modification),
    primary_outcome_significant = factor(primary_outcome_significant,
                        levels = c("0", "1"),
                        labels = c("No", "Yes")),
    statistically_significant_difference_for_cci = 
      factor(statistically_significant_difference_for_cci,
                        levels = c("0", "1"),
                        labels = c("No", "Yes")),
    between_group_difference_of_cci = as.numeric(
      signif(as.numeric(between_group_difference_of_cci), 3)),
    between_group_difference_of_cci_12 = factor(between_group_difference_of_cci_12,
                                                 levels = c("0", "1"),
                                                 labels = c("No", "Yes")),
    mean_or_median_used = factor(mean_or_median_used,
                        levels = c("0", "1"),
                        labels = c("Mean", "Median")),
    cci_for_ss_used = factor(cci_for_ss_used,
                        levels = c("0", "1"),
                        labels = c("No", "Yes")),
    if_cci_used_specification_of_ss_calculation = 
      as.character(if_cci_used_specification_of_ss_calculation),
  study_must_be_excluded_e_g_protocol_whose_study_has_been_conducted = 
    factor(study_must_be_excluded_e_g_protocol_whose_study_has_been_conducted,
                       levels = c("0", "1"),
                       labels = c("No", "Yes")),
  evaluation_unclear = factor(evaluation_unclear, 
                              levels = c("0", "1"), labels = c("No", "Yes")),
  comments = as.character(comments),
  doi_protocol = as.character(doi_protocol),
  protocol_published = factor(protocol_published,
                        levels = c("0", "1", "2"),
                        labels = c("Neither published nor registered", "Published as paper", "Registered without publication")),
  cci_1st_ep_in_protocol = factor(cci_1st_ep_in_protocol,
                        levels = c("0", "1"),
                        labels = c("No", "Yes")),
  cci_in_ss_calculation_of_protocol = factor(cci_in_ss_calculation_of_protocol,
                        levels = c("0", "1"),
                        labels = c("No", "Yes")),
  specification_of_ss_calculation = as.character(specification_of_ss_calculation),
  study_superiority_non_inferiority = factor(type_of_study,
                        levels = c("0", "1"),
                        labels = c("Superiority", "Non-inferiority")),
  definition_of_non_inf_margin = as.character(definition_of_non_inf_margin)) %>% 
  relocate(paper_type, .after=protocol) %>%
  relocate(protocol_published_original_column, .after=paper_type) %>%
  relocate(study_type, .after=protocol_published_original_column) %>%
  relocate(continent, .after=post_hoc_secondary_analysis) %>%
  relocate(discipline, .after=continent) %>%
  relocate(cci_report_in_results, .after=sample_size) %>%
  relocate(cci_ep, .after=cci_report_in_results) 
  
#deleting useless variables
extracted_23_RCT$discipine <- NULL
extracted_23_RCT$type_of_study <- NULL
extracted_23_RCT$pilot_feasibility <- NULL
extracted_23_RCT$discipline_cleaned <- NULL
extracted_23_RCT$study_superiority_non_inferiority <- NULL
extracted_23_RCT$cci_1st_ep <- NULL
extracted_23_RCT$cci_2nd_ep <- NULL
extracted_23_RCT$cci_expl_ep <- NULL
extracted_23_RCT$results_of_the_protocol_already_published <- NULL
extracted_23_RCT$published_results_included_in_the_list <- NULL
extracted_23_RCT$continent_last_author <- NULL
extracted_23_RCT$cci_reported_in_results <- NULL
#saving excel and csv
write_xlsx(extracted_23_RCT, "Data_23_RCT_cleaned_2025_09_21.xlsx")
write_csv(extracted_23_RCT, "Data_23_RCT_cleaned_2025_09_21.csv")

#table

#ensure factor variables are factors
discrete_variables <- c("paper_type", 
                       # "protocol",
                       "protocol_published_original_column",
                        "study_type",
                        "pooled_analysis",
                        "post_hoc_secondary_analysis",
                        "discipline",
                        "continent",
                        "cci_ep",
                        "cci_report_in_results",
                        "cci_modification",
                        "primary_outcome_significant",
                        "statistically_significant_difference_for_cci",
                        "between_group_difference_of_cci_12",
                        "mean_or_median_used",
                       "cci_for_ss_used",
                        "study_must_be_excluded_e_g_protocol_whose_study_has_been_conducted",
                        "evaluation_unclear",
                       "protocol_published",
                       "cci_1st_ep_in_protocol",
                        "cci_in_ss_calculation_of_protocol")
extracted_23_RCT[, discrete_variables] <- lapply(
  extracted_23_RCT[, discrete_variables], factor)
continuous_variables <- c("sample_size", "between_group_difference_of_cci"
)
#ensure all continuous variables are numeric
extracted_23_RCT[, continuous_variables] <- lapply(
  extracted_23_RCT[, continuous_variables], as.numeric)
#Create a list of variables for the table
var_list_CCI_23 <- list(
  paper_type ~"Paper type", 
  #protocol ~"Protocol", 
  #protocol_published_original_columnn~"Protocol publication status", 
  study_type ~"Study type", 
  #study_superiority_non_inferiority ~"Study superiority/ non-inferiority",
  pooled_analysis ~"pooled_analysis",
  post_hoc_secondary_analysis ~"Post-hoc secondary analysis",
  discipline ~"Discipline",
  continent ~"Continent", 
  sample_size ~ "Sample size",
  cci_ep ~"CCI use as endpoint",
  cci_report_in_results ~ "CCI report in results",
  cci_modification ~"CCI modification",
  specification_of_modification ~"CCI modification specification",
  primary_outcome_significant ~ "Primary outcome significant",
  statistically_significant_difference_for_cci ~"CCI difference significant",
  between_group_difference_of_cci ~"CCI difference [pts]",
  mean_or_median_used ~ "Mean or median used",
  cci_for_ss_used ~ "CCI used for sample size calculation",
  if_cci_used_specification_of_ss_calculation ~"Details on sample size calculation with CCI",
  study_must_be_excluded_e_g_protocol_whose_study_has_been_conducted ~ "Study exclusion",
  #evaluation_unclear ~"evaluation_unclear",
 #protocol_published ~"Protocol publication",
  cci_1st_ep_in_protocol ~"CCI as primary endpoint in protocol", 
cci_in_ss_calculation_of_protocol ~"CCI used for sample size calculation in protocol",
specification_of_ss_calculation ~ "Details on sample size calculation with CCI in protocol",
definition_of_non_inf_margin ~"Definition of non-inferiority margin in protocol"
)
#table code
Table_descriptive_23_RCT <- extracted_23_RCT %>% filter(study_must_be_excluded_e_g_protocol_whose_study_has_been_conducted ==0) %>% 
  select(cci_ep, paper_type,
         #protocol,
         #protocol_published_original_column,
         study_type ,
         #study_superiority_non_inferiority,
         pooled_analysis ,post_hoc_secondary_analysis,discipline ,
         continent,sample_size,cci_report_in_results , cci_modification,
         specification_of_modification,primary_outcome_significant ,
         statistically_significant_difference_for_cci ,between_group_difference_of_cci_12,
         between_group_difference_of_cci ,mean_or_median_used ,
         cci_for_ss_used,if_cci_used_specification_of_ss_calculation,
         study_must_be_excluded_e_g_protocol_whose_study_has_been_conducted,
         protocol_published,
         cci_1st_ep_in_protocol, 
         cci_in_ss_calculation_of_protocol,
         specification_of_ss_calculation,
         definition_of_non_inf_margin
  ) %>%
  tbl_summary(
    by = protocol_published,
    label = var_list_CCI_23,
    statistic = list(
      c("sample_size", "between_group_difference_of_cci") ~ "{median} ({p25}-{p75})",
      c( "paper_type",
         #"protocol",
         #protocol_published_original_column,
         #protocol_published" ,
         "study_type" ,
         # "study_superiority_non_inferiority",
         "pooled_analysis" ,"post_hoc_secondary_analysis","discipline" ,
         "continent","cci_report_in_results" , "cci_modification",
         "specification_of_modification","primary_outcome_significant" ,
         "between_group_difference_of_cci_12",
         "statistically_significant_difference_for_cci"  ,"mean_or_median_used" ,
         "cci_for_ss_used", "if_cci_used_specification_of_ss_calculation",
         "study_must_be_excluded_e_g_protocol_whose_study_has_been_conducted",
         "cci_1st_ep_in_protocol", 
         "cci_in_ss_calculation_of_protocol",
         "specification_of_ss_calculation",
         "definition_of_non_inf_margin"
      ) ~ "{p}% ({n})"),
    digits = list(
      all_categorical() ~ c(1,0),
      all_continuous() ~ c(2,2)# 0 decimals for counts, 1 for percentages
    ), missing = "no"
  ) %>%
  add_p() %>%
  add_overall()


#Save Table_descriptive
Table_descriptive_23_RCT_path<-paste(getwd(), "/Output/Table/Table_descriptive_23_RCT_RCT.docx", sep = "")
Table_descriptive_23_RCT%>%as_flex_table()%>%flextable::save_as_docx(Table_descriptive_23_RCT, path = "tables/Table_descriptive_23_RCT.docx")


#plotting
library(ggplot2)
#Source my theme for plotting
source("code/Functions.R")

#perform ggplot show tendency of publications accross years
plot_tendency_across_years <- ggplot(extracted_23_RCT, aes(x = publication_year)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Number of Publications per Year",
       x = "Publication Year",
       y = "Count of Publications") +
  theme_JRR() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#save plot
ggsave("plots/plot_tendency_across_years.png", plot = plot_tendency_across_years, width = 8, height=5, dpi = 1200)

#perform ggplot show number patients accross years
plot_patients_across_years <- ggplot(extracted_23_RCT, aes(x = publication_year, y = as.numeric(sample_size))) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  labs(title = "Number of Patients per Year",
       x = "Publication Year",
       y = "Number of Patients") +
  theme_JRR() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#save plot
ggsave("plots/plot_patients_across_years.png", plot = plot_patients_across_years, width = 8, height=5, dpi = 1200)

#perform ggplot show number patients accross years
# Count number of studies per Subgroup and GroupB
study_counts <- extracted_23_RCT %>%
  mutate(mismatch_CCI_study_protocol = case_when(
    cci_ep == "CCI primary endpoint" & cci_1st_ep_in_protocol == "No" ~ "CCI not primary endpoint in protocol",
    cci_ep == "CCI primary endpoint" & cci_1st_ep_in_protocol == "Yes" ~ "CCI also primary endpoint in protocol"
  )) %>% 
  group_by(mismatch_CCI_study_protocol, cci_for_ss_used, cci_in_ss_calculation_of_protocol, cci_report_in_results) %>%
  summarise(NumStudies = n_distinct(article_id), .groups = "drop")

  # Create the bar plot
 plot1 <-  ggplot(study_counts, aes(x = mismatch_CCI_study_protocol, y = NumStudies, fill = cci_in_ss_calculation_of_protocol)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = "Number of Studies by cci_in_ss_calculation_of_protocol and GroupB",
      x = "mismatch_CCI_study_protocol",
      y = "Number of Studies",
      fill = "cci_for_ss_used B"
    ) +
    theme_minimal()
  
  #save plot
ggsave("plots/mismatch_and_CCI_use_SS_calc_protocol.png", plot = plot1, width = 8, height=5, dpi = 1200)

# Create the bar plot
plot2 <-  ggplot(study_counts, aes(x = mismatch_CCI_study_protocol, y = NumStudies, fill = cci_for_ss_used )) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Number of Studies by mismatch_CCI_study_protocol and cci_for_ss_used",
    x = "mismatch_CCI_study_protocol",
    y = "Number of Studies",
    fill = "cci_for_ss_used "
  ) +
  theme_minimal()

#save plot
ggsave("plots/mismatch_and_CCI_ss_calc_study.png", plot = plot2, width = 8, height=5, dpi = 1200)

# Create the bar plot
plot3 <-  ggplot(study_counts, aes(x = mismatch_CCI_study_protocol, y = NumStudies, fill = cci_report_in_results )) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Number of Studies by mismatch_CCI_study_protocol and cci_report_in_results",
    x = "mismatch_CCI_study_protocol",
    y = "Number of Studies",
    fill = "cci_report_in_results "
  ) +
  theme_minimal()

#save plot
ggsave("plots/mismatch_and_CCI_report_results.png", plot = plot3, width = 8, height=5, dpi = 1200)




nrow(extracted_23_RCT%>% filter(primary_outcome_significant==1))
nrow(extracted_23_RCT%>% filter(primary_outcome_significant==1 & between_group_difference_of_cci_12==1))

