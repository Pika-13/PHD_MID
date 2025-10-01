#project on MID with Fariba
#112 RCT analysis
#Load necessary packages
lapply(c("tidyverse", "data.table","utils", "writexl", "readxl",
         "ggpubr", "scales", "flextable", "janitor", "labelled", "gtsummary", "dplyr"), require,
       character.only = TRUE)
#---------------------
#reading 24 RCT excel file, cleaning, tidying
extracted_112_RCT <- janitor::clean_names(read_xlsx("Data_112_RCT_2025_09_21.xlsx",
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
extracted_112_RCT$discipine <- NULL
extracted_112_RCT$type_of_study <- NULL
extracted_112_RCT$pilot_feasibility <- NULL
extracted_112_RCT$discipline_cleaned <- NULL
extracted_112_RCT$study_superiority_non_inferiority <- NULL
extracted_112_RCT$misc_2 <- NULL
extracted_112_RCT$cci_1st_ep <- NULL
extracted_112_RCT$cci_2nd_ep <- NULL
extracted_112_RCT$cci_expl_ep <- NULL
extracted_112_RCT$results_of_the_protocol_already_published <- NULL
extracted_112_RCT$published_results_included_in_the_list <- NULL
extracted_112_RCT$cci_reported_in_results <- NULL
extracted_112_RCT$continent_last_author <- NULL
#saving excel and csv
write_xlsx(extracted_112_RCT, "Data_112_RCT_cleaned_2025_09_21.xlsx")
write_csv(extracted_112_RCT, "Data_112_RCT_cleaned_2025_09_21.csv")

#----------------------------------------------
#----------------------------------------------
#summary table
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
extracted_112_RCT[, discrete_variables] <- lapply(
  extracted_112_RCT[, discrete_variables], factor)
continuous_variables <- c("sample_size", "between_group_difference_of_cci"
)
#ensure all continuous variables are numeric
extracted_112_RCT[, continuous_variables] <- lapply(
  extracted_112_RCT[, continuous_variables], as.numeric)

#List with all renamed variables
var_list_CCI_112 <- list(
  paper_type ~"Paper type", 
 # protocol ~"Protocol", 
 # protocol_published ~"Protocol publication",
 #protocol_published_original_columnn~"Protocol publication status", 
  study_type ~"Study type", 
#  study_superiority_non_inferiority ~"Study superiority/ non-inferiority",
  pooled_analysis ~"pooled_analysis",
  post_hoc_secondary_analysis ~"Post-hoc secondary analysis",
  discipline ~"Discipline",
  continent ~"Continent", 
  sample_size ~ "Sample size",
 # cci_ep ~"CCI use as endpoint",
  cci_report_in_results ~ "CCI report in results",
  cci_modification ~"CCI modification",
  specification_of_modification ~"CCI modification specification",
  primary_outcome_significant ~ "Primary outcome significant",
  statistically_significant_difference_for_cci ~"CCI difference significant",
between_group_difference_of_cci_12~"CCI difference ≥ 12 pts",
  between_group_difference_of_cci ~"CCI difference [pts]",
  mean_or_median_used ~ "Mean or median used",
  cci_for_ss_used ~ "CCI used for sample size calculation",
  #if_cci_used_specification_of_ss_calculation ~"Details on sample size calculation with CCI",
  study_must_be_excluded_e_g_protocol_whose_study_has_been_conducted ~ "Study exclusion"
  #evaluation_unclear ~"evaluation_unclear"
)
#table code
Table_descriptive_112_RCT <- extracted_112_RCT %>% 
  select(cci_ep, paper_type,
         #protocol,protocol_published ,
         study_type ,
         #study_superiority_non_inferiority,
         #protocol_published_original_columnn~"Protocol publication status", 
         pooled_analysis ,post_hoc_secondary_analysis,discipline ,
         continent,sample_size,cci_report_in_results , cci_modification,
         specification_of_modification,primary_outcome_significant ,
         statistically_significant_difference_for_cci ,between_group_difference_of_cci_12,
         between_group_difference_of_cci ,mean_or_median_used ,
         cci_for_ss_used,
         study_must_be_excluded_e_g_protocol_whose_study_has_been_conducted) %>%
  tbl_summary(
    by = cci_ep,
    label = var_list_CCI_112,
    statistic = list(
      c("sample_size", "between_group_difference_of_cci") ~ "{median} ({p25}-{p75})",
      c( "paper_type",
         #"protocol","protocol_published" ,  
         #protocol_published_original_columnn~"Protocol publication status", 
         "study_type" ,
        # "study_superiority_non_inferiority",
         "pooled_analysis" ,"post_hoc_secondary_analysis","discipline" ,
         "continent","cci_report_in_results" , "cci_modification",
         "specification_of_modification","primary_outcome_significant" ,
        "between_group_difference_of_cci_12",
         "statistically_significant_difference_for_cci"  ,"mean_or_median_used" ,
         "cci_for_ss_used",
         "study_must_be_excluded_e_g_protocol_whose_study_has_been_conducted") ~ "{p}% ({n})"),
    digits = list(
      all_categorical() ~ c(1,0),
      all_continuous() ~ c(2,2)# 0 decimals for counts, 1 for percentages
    ), missing = "no"
  ) %>%
  add_p() %>%
  add_overall()


#Save Table_descriptive
Table_descriptive_112_RCT_path<-paste(getwd(), "/Output/Table/Table_descriptive_112_RCT.docx", sep = "")
Table_descriptive_112_RCT%>%as_flex_table()%>%flextable::save_as_docx(Table_descriptive_112_RCT, path = "tables/Table_descriptive_112_RCT.docx")

#----------------------------------------------
#----------------------------------------------
#plotting
library(ggplot2)
#Source my theme for plotting
source("code/Functions.R")

#perform ggplot show tendency of publications accross years
plot_tendency_across_years <- ggplot(extracted_112_RCT, aes(x = publication_year)) +
  geom_bar(fill = "skyblue", color = "white") +
  labs(title = "Number of Publications per Year",
       x = "Publication Year",
       y = "Count of Publications") +
  theme_JRR() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#save plot
ggsave("plots/plot_112_tendency_across_years.png", plot = plot_tendency_across_years, width = 8, height=5, dpi = 1200)

#perform ggplot show number patients accross years
plot_patients_across_years <- ggplot(extracted_112_RCT, aes(x = publication_year, y = as.numeric(sample_size))) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "white") +
  labs(title = "Number of Patients per Year",
       x = "Publication Year",
       y = "Number of Patients") +
  theme_JRR() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#save plot
ggsave("plots/plot_112_patients_across_years.png", plot = plot_patients_across_years, width = 8, height=5, dpi = 1200)

#perform ggplot show distribution studies according to 
####### 1) cci 1/2/exp,
####### 2) cci for ss
####### 3) cci for ss -> comments

plot_patients_across_years <- 
  extracted_112_RCT %>% summarise(
    total_studies = n(),
    cci_1st_ep_yes = sum(cci_1st_ep == "Yes", na.rm = TRUE),
    cci_2nd_ep_yes = sum(cci_2nd_ep == "Yes", na.rm = TRUE),
    cci_expl_ep_yes = sum(cci_expl_ep == "Yes", na.rm = TRUE),
    cci_for_ss_used_yes = sum(cci_for_ss_used == "Yes", na.rm = TRUE)
  ) %>% 
  
%>% 
  ggplot( aes(x = publication_year, y = as.numeric(sample_size))) +
  geom_bar(stat = "identity", fill = , color = "white") +
  labs(title = "Number of Patients per Year",
       x = "Publication Year",
       y = "Number of Patients") +
  theme_JRR() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#save plot
ggsave("plots/plot_112_patients_across_years.png", plot = plot_patients_across_years, width = 8, height=5, dpi = 1200)

nrow(extracted_112_RCT%>% filter(primary_outcome_significant==1))
nrow(extracted_112_RCT%>% filter(primary_outcome_significant==1 & between_group_difference_of_cci_12==1))

