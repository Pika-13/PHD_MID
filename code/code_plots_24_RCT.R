#project on MID with Fariba
#24 RCT analysis
#Load necessary packages
lapply(c("tidyverse", "data.table", "dplyr","utils", "writexl", "readxl",
         "ggpubr", "scales", "flextable", "janitor"), require,
       character.only = TRUE)
#---------------------
#reading 24 RCT excel file, cleaning, tidying
extracted_24_RCT <- janitor::clean_names(read_xlsx("Data_24_RCT_Joana_2025_09_17.xlsx",
                              col_names = TRUE)[-1, ])%>% 
  mutate(
    article_ID = as.character(article_id),
    Title =  as.character(title),
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
    protocol_published = factor(case_when(
      protocol=="Protocol" &
        results_of_the_protocol_already_published =="1" & 
        published_results_included_in_the_list == "1" ~ "Study published, listed",
      results_of_the_protocol_already_published =="1" &
        protocol=="Protocol" & 
        published_results_included_in_the_list == "0" ~ "Study published, not listed",
      protocol=="Protocol" & 
        results_of_the_protocol_already_published =="0" ~ "Study not published yet",
      protocol=="Not a protocol" ~ NA)),
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
  definition_of_non_inf_margin = as.character(definition_of_non_inf_margin))
#deleting useless variables
extracted_24_RCT$discipine <- NULL
extracted_24_RCT$type_of_study <- NULL
extracted_24_RCT$pilot_feasibility <- NULL
extracted_24_RCT$discipline_cleaned <- NULL
extracted_24_RCT$study_superiority_non_inferiority <- NULL

#saving excel and csv
write_xlsx(extracted_24_RCT, "Data_24_RCT_cleaned_2025_09_21.xlsx")
write_csv(extracted_24_RCT, "Data_24_RCT_cleaned_2025_09_21.csv")



extracted_112_RCT$discipine <- NULL
extracted_112_RCT$type_of_study <- NULL
extracted_112_RCT$pilot_feasibility <- NULL
extracted_112_RCT$discipline_cleaned <- NULL
extracted_112_RCT$study_superiority_non_inferiority <- NULL


#plotting
library(ggplot2)
#Source my theme for plotting
source("code/Functions.R")

#perform ggplot show tendency of publications accross years
plot_tendency_across_years <- ggplot(extracted_24_RCT, aes(x = publication_year)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Number of Publications per Year",
       x = "Publication Year",
       y = "Count of Publications") +
  theme_JRR() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#save plot
ggsave("plots/plot_tendency_across_years.png", plot = plot_tendency_across_years, width = 8, height=5, dpi = 1200)

#perform ggplot show number patients accross years
plot_patients_across_years <- ggplot(extracted_24_RCT, aes(x = publication_year, y = as.numeric(sample_size))) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  labs(title = "Number of Patients per Year",
       x = "Publication Year",
       y = "Number of Patients") +
  theme_JRR() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#save plot
ggsave("plots/plot_patients_across_years.png", plot = plot_patients_across_years, width = 8, height=5, dpi = 1200)

nrow(extracted_24_RCT%>% filter(primary_outcome_significant==1))
nrow(extracted_24_RCT%>% filter(primary_outcome_significant==1 & between_group_difference_of_cci_12==1))

