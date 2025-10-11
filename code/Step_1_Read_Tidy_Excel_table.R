#project on MID with Fariba
#Whole list RCT analysis

#Date creation: 2025-10-01
#Last update: 2025-10-...

#Load necessary packages
lapply(c("tidyverse", "data.table","utils", "writexl", "readxl",
         "ggpubr", "scales", "flextable", "janitor", "labelled","forcats", "gtsummary", "dplyr"), require,
       character.only = TRUE)
#---------------------
#reading excel file, cleaning, tidying
compared_RCT_2025_10_10 <- janitor::clean_names(
  read_xlsx("database/Revised_Data_original_Joana_2025_10_09_compared.xlsx",col_names = TRUE)[-1, ]) %>% 
  mutate(specification_of_ss_calculation_protocol = case_when(
    protocol_published %in% c("") ~ NA_character_,
    specification_of_ss_calculation == "na" ~ NA_character_,
    specification_of_ss_calculation == "nm" ~ "Not mentioned",
    specification_of_ss_calculation != "na" & 
      !is.na(specification_of_ss_calculation) ~ as.character(specification_of_ss_calculation))) %>% 
  select(-specification_of_ss_calculation) %>%
    mutate(ss_calculation_protocol = factor(case_when(
    protocol_published %in% c("0", "na") ~ NA_character_,
    is.na(protocol_published) ~ NA_character_,
    protocol_published%in% c("1", "2", "3") & is.na(specification_of_ss_calculation_protocol) ~ "0",
      protocol_published%in% c("1", "2", "3") & specification_of_ss_calculation_protocol == "nm" ~ "0",
      protocol_published%in% c("1", "2", "3") & specification_of_ss_calculation_protocol == "na" ~ "0",
      protocol_published%in% c("1", "2", "3") & 
        (specification_of_ss_calculation_protocol != "nm" & specification_of_ss_calculation_protocol != "na") ~ "1"),
      levels = c("1","0"),  labels = c("Yes", "No")),
    specification_of_ss_calculation = case_when(
      protocol==1 ~ NA_character_,
      is.na(specification_of_ss_calculation_in_paper) ~ NA_character_,
      specification_of_ss_calculation_in_paper == "na" ~ NA_character_,
      specification_of_ss_calculation_in_paper != "na" & 
        !is.na(specification_of_ss_calculation_in_paper) ~ as.character(specification_of_ss_calculation_in_paper))) %>% 
  select(-specification_of_ss_calculation_in_paper) %>%
  mutate(ss_calculation = case_when(
    protocol==1 ~ NA_character_,
    protocol==0 & is.na(specification_of_ss_calculation) ~ "0",
      protocol==0 & specification_of_ss_calculation == "nm" ~ "0",
    protocol==0 & specification_of_ss_calculation == "na" ~ "0",
    specification_of_ss_calculation != "na" & 
        specification_of_ss_calculation != "nm" &
        !is.na(specification_of_ss_calculation) ~ "1")) %>%
  mutate(
    ss_calculation = factor(ss_calculation,levels = c("1","0"),  labels = c("Yes", "No"))    ,
    definition_of_non_inf_margin = case_when(
      protocol==1 ~ NA_character_,
      definition_of_non_inf_margin == "na" ~ NA_character_,
      definition_of_non_inf_margin == "nm" ~ NA_character_,
      definition_of_non_inf_margin != "na" & 
        definition_of_non_inf_margin != "nm"&
        !is.na(definition_of_non_inf_margin) ~ as.character(definition_of_non_inf_margin)),
    non_inf_margin= case_when(
      type_of_study == "1" & is.na(definition_of_non_inf_margin) ~ 0,
      type_of_study== "1" & 
        !is.na(definition_of_non_inf_margin) ~ 1)) %>% 
  mutate(non_inf_margin = factor(non_inf_margin,
                                     levels = c("0", "1"), labels = c("No", "Yes")),  
     type_of_study_sup_non_inf = case_when(
      protocol==1 ~ NA_character_,
      type_of_study == "na"~ NA_character_,  
        is.na(type_of_study) ~ NA_character_,
      type_of_study == "nm"  ~ "nm", 
      type_of_study == "0"  ~ "0", 
      type_of_study == "1"  ~ "1",
      type_of_study == "3"  ~ "3")) %>% 
  mutate(
    type_of_study_sup_non_inf = factor(type_of_study_sup_non_inf,
                                       levels = c("0", "1", "3", "nm"),
                                       labels = c("Superiority trial", "Non-inferiority trial",
                                                  "Equivalence trial", "Not mentioned"))) %>% 
  select(-type_of_study) %>%
  mutate(
    non_inf_margin= case_when(
      protocol==1 ~ NA_character_,
      type_of_study_sup_non_inf=="Non-inferiority trial" & definition_of_non_inf_margin =="nm" ~ "0",
      type_of_study_sup_non_inf=="Non-inferiority trial" & is.na(definition_of_non_inf_margin) ~ "0",
      type_of_study_sup_non_inf=="Non-inferiority trial" & definition_of_non_inf_margin != "nm" ~"1"))%>% 
  mutate(
      non_inf_margin = factor(non_inf_margin,
      levels = c("0", "1"),
      labels = c("No", "Yes")),
 definition_of_non_inf_margin_protocol = case_when(
   protocol==1 ~ NA_character_,
   definition_of_non_inf_margin_protocol == "na" ~ NA_character_,
   definition_of_non_inf_margin_protocol == "nm" ~ NA_character_,
   definition_of_non_inf_margin_protocol != "na" & 
     definition_of_non_inf_margin_protocol != "nm"&
     !is.na(definition_of_non_inf_margin_protocol) ~ as.character(definition_of_non_inf_margin_protocol)),
    non_inf_margin_protocol= case_when(
      type_of_study_protocol == "1" & is.na(definition_of_non_inf_margin_protocol) ~ 0,
      type_of_study_protocol=="1" & definition_of_non_inf_margin_protocol== "Not mentioned" ~ 0,
      type_of_study_protocol== "1" & 
        !is.na(definition_of_non_inf_margin_protocol) & 
        definition_of_non_inf_margin_protocol!= "na" & 
        definition_of_non_inf_margin_protocol != "Not mentioned" ~ 1)) %>% 
  mutate(
    non_inf_margin_protocol = factor(non_inf_margin_protocol,
      levels = c("0", "1"), labels = c("No", "Yes")),
    article_id = as.character(article_id) ,
    title =  as.character(title),
    publication_year = as.integer(publication_year)) %>% 
  #  mutate(vol_nr=as.character(vol_nr),                                                      
  #  issue_nr = as.character(issue_nr),
  #  journal=as.character(journal),
  #  paper_type = factor(case_when(
  #    str_detect(misc_2, "Article") ~ "Article",
  #    str_detect(misc_2, "Conference Paper") ~ "Conference Paper",
  #    .default = "Unknown"), levels = c("Unknown", "Article", "Conference Paper"))) %>% 
  #select(-misc_2) %>%
  mutate(
    web_url = as.character(web_url),
    doi = as.character(doi),
    trial_nr= as.character(trial_nr),
    protocol= factor(protocol, levels=c("0", "1"), labels = c("Not a protocol", "Protocol")),
    protocol_published_original_column = factor(case_when(
      protocol=="Protocol" &
        results_of_the_protocol_already_published =="1" & 
        published_results_included_in_the_list == "1" ~ "Study published, listed",
      results_of_the_protocol_already_published =="1" &
        protocol=="Protocol" & 
        published_results_included_in_the_list == "0" ~ "Study published, not listed",
      protocol=="Protocol" & 
        results_of_the_protocol_already_published =="0" ~ "Study not published yet"),
      levels = c("Study published, listed", 
                 "Study published, not listed", 
                 "Study not published yet")),
    study_type_pilot = factor(pilot_feasibility, levels = c("0","1"),
                              labels = c("Not pilot study", "Pilot/ Safety study"))) %>% 
  select(-pilot_feasibility) %>%
  mutate(
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
                                  "Asia", "Australia/ New Zealand","Africa"))) %>% 
  select(-continent_last_author) %>%
  mutate(
    exclusion_reason = factor(exclusion_because_of, levels= c("1", "2", "3", "4", "5", "6"),
                              labels = c("Duplicate publication", "Pilot/ feasibility study",
                                         "No CCI used", "Protocol of published trial", 
                                         "Not an RCT", "Secondary analysis of included trials"))) %>% 
  select(-exclusion_because_of) %>%
  mutate(
    sample_size = as.numeric(sample_size),
    cci_1st_ep = case_when(
      protocol=="Protocol" ~ NA_character_,
      cci_1st_ep == 0 ~ "0",
      cci_1st_ep == 1 ~ "1"),
    cci_1st_ep = factor(cci_1st_ep,
                        levels = c("0", "1", "nm"),
                        labels = c("No", "Yes", "Not mentioned")),
    cci_2nd_ep = case_when(
      protocol=="Protocol" ~ NA_character_,
      cci_2nd_ep == 0 ~ "0",
      cci_2nd_ep == 1 ~ "1"),
    cci_2nd_ep = factor(cci_2nd_ep,
                        levels = c("0", "1", "nm"),
                        labels = c("No", "Yes", "Not mentioned")),
    cci_expl_ep = case_when(
      protocol=="Protocol" ~ NA_character_,
      cci_expl_ep == 0 ~ "0",
      cci_expl_ep == 1 ~ "1"),
    cci_expl_ep = factor(cci_expl_ep,
      levels = c("0", "1", "nm"),
      labels = c("No", "Yes", "Not mentioned")),
    cd_cci_reported_in_results = case_when(
      protocol=="Protocol" ~ NA_character_,
      cd_cci_reported_in_results == 0 ~ "0",
      cd_cci_reported_in_results == 1 ~ "1"),
    cd_cci_reported_in_results = factor(cd_cci_reported_in_results,
                                        levels = c("0", "1", "nm"),
                                        labels = c("No", "Yes", "Not mentioned")),
    cci_ep = case_when(
      protocol=="Protocol" ~ NA_character_,
      (cci_1st_ep %in% c("No", "nm")|is.na(cci_1st_ep)) & 
                                         (cci_2nd_ep %in% c("No", "nm")|is.na(cci_2nd_ep)) &
                                         (cci_expl_ep %in% c("No", "nm") |is.na(cci_expl_ep))~ "CCI not an endpoint",
                              cci_1st_ep %in% c("No", "nm") & is.na(cci_2nd_ep) &
                                cci_expl_ep %in% c("No", "nm") ~ "CCI not an endpoint",
                              cci_1st_ep == "Yes" & cci_2nd_ep %in% c("No", "nm") ~ "CCI primary endpoint",
                              cci_1st_ep == "Yes" & is.na(cci_2nd_ep) ~ "CCI primary endpoint",
                              cci_1st_ep %in% c("No", "nm") & cci_2nd_ep == "Yes" ~ "CCI secondary endpoint",
                              cci_1st_ep == "Yes" & cci_2nd_ep == "Yes" ~ "CCI both primary and secondary endpoint",
                              cci_expl_ep == "Yes" ~ "CCI exploratory endpoint"),
    cci_ep = factor(cci_ep,levels = c("CCI primary endpoint", "CCI both primary and secondary endpoint",
                                        "CCI secondary endpoint",
                                        "CCI exploratory endpoint", "CCI not an endpoint"),
                    labels = c("CCI primary endpoint","CCI primary endpoint", "CCI non-primary endpoint",
                               "CCI non-primary endpoint","CCI not an endpoint")),
    cci_modification = factor(cci_modification,
                              levels = c("0", "1"),
                              labels = c("No", "Yes")),
    specification_of_modification = as.character(specification_of_modification),
    primary_outcome_significant = case_when(
      protocol=="Protocol" ~ NA_character_,
      primary_outcome_significant == 0 ~ "0",
      primary_outcome_significant == 1 ~ "1"),
    primary_outcome_significant = factor(primary_outcome_significant,
      levels = c("0", "1"),
      labels = c("No", "Yes")),
    statistically_significant_difference_for_cci =case_when(
      protocol=="Protocol" ~ NA_character_,
      statistically_significant_difference_for_cci == 0 ~ "0",
      statistically_significant_difference_for_cci == 1 ~ "1"),
    statistically_significant_difference_for_cci = factor(statistically_significant_difference_for_cci,
      levels = c("0", "1"),
      labels = c("No", "Yes")),
    between_group_difference_of_cci = as.numeric(
      signif(as.numeric(between_group_difference_of_cci), 3)),
    between_group_difference_of_cci_12 = factor(between_group_difference_of_cci_12,
                                                levels = c("0", "1"),
                                                labels = c("No", "Yes")),
    mean_or_median_used = case_when(
      protocol=="Protocol" ~ NA_character_,
      mean_or_median_used == 0 ~ "0",
      mean_or_median_used == 1 ~ "1",
      mean_or_median_used=="nm" ~"nm"),
    mean_or_median_used = factor(mean_or_median_used,
                                 levels = c("0", "1", "nm"),
                                 labels = c("Mean", "Median", "Not mentioned")),
    study_type_mentioned = case_when(
      protocol=="Protocol" ~ NA_character_,
      study_type_mentioned == 0 ~ "0",
      study_type_mentioned == 1 ~ "1"),
    study_type_mentioned = factor(study_type_mentioned,
      levels = c("0", "1"),
      labels = c("No", "Yes")),
    type_of_study_sup_non_inf = case_when(
      protocol=="Protocol" ~ NA_character_,
      protocol== "Not a protocol" & is.na(type_of_study_sup_non_inf) ~"Not mentioned",
      type_of_study_sup_non_inf == "na" ~"Not mentioned",
      type_of_study_sup_non_inf == "nm" ~"Not mentioned",
      study_type_mentioned== "Yes" & type_of_study_sup_non_inf=="Non-inferiority trial"~"Non-inferiority trial (explicited)",
      study_type_mentioned== "No" & type_of_study_sup_non_inf=="Non-inferiority trial"~"Non-inferiority trial (assumed)",
      study_type_mentioned== "Yes" & type_of_study_sup_non_inf=="Superiority trial"~"Superiority trial (explicited)",
      study_type_mentioned== "No" & type_of_study_sup_non_inf=="Superiority trial"~"Superiority trial (assumed)"),
    type_of_study_sup_non_inf = factor(type_of_study_sup_non_inf,
      levels = c("Non-inferiority trial (explicited)", "Non-inferiority trial (assumed)",
                 "Superiority trial (explicited)","Superiority trial (assumed)", "Not mentioned"),
      labels = c("Non-inferiority trial (explicited)", "Non-inferiority trial (assumed)",
                 "Superiority trial (explicited)","Superiority trial (assumed)", "Not mentioned")),
     cci_used_for_ss_calculation = case_when(
       protocol=="Protocol" ~ NA_character_,
       ss_calculation == "No" ~ NA_character_,
       cci_used_for_ss_calculation_in_paper == 0 ~ "0",
       cci_used_for_ss_calculation_in_paper == 1 ~ "1",
       TRUE ~NA_character_ ))%>% 
  mutate(
    cci_used_for_ss_calculation = factor(cci_used_for_ss_calculation,
    levels = c("0", "1"),
    labels = c("No", "Yes"))) %>% 
  select(-cci_used_for_ss_calculation_in_paper) %>%
  mutate(
     study_must_be_excluded = 
      case_when(
         study_must_be_excluded_e_g_protocol_whose_study_has_been_conducted == 0 ~ "0",
         study_must_be_excluded_e_g_protocol_whose_study_has_been_conducted == 1 ~ "1")) %>% 
  mutate(
     study_must_be_excluded = factor(study_must_be_excluded,
         levels = c("0", "1"),
         labels = c("No", "Yes"))) %>% 
  select(-study_must_be_excluded_e_g_protocol_whose_study_has_been_conducted) %>%
  mutate(
    comments = as.character(comments),
    doi_protocol = as.character(doi_protocol),
    protocol_published = factor(protocol_published,
                                levels = c("1","3", "2", "0"),
                                labels = c("Protocol published", "Protocol unpublished", 
                                           "No protocol,registry", "Neither protocol nor registry")),
    study_type_mentioned_protocol= factor(study_type_mentioned_protocol,
                                          levels = c(0,1),
                                          labels = c("No", "Yes")),
    type_of_study_sup_non_inf_protocol = case_when(
      protocol_published=="Neither protocol nor registry" ~ NA_character_,
      type_of_study_protocol =="nm"~"Not mentioned",
      study_type_mentioned_protocol== "Yes" & type_of_study_protocol =="3"~"Equivalence trial (explicited)",
      study_type_mentioned_protocol== "No" & type_of_study_protocol=="3"~"Equivalence trial (assumed)",
      study_type_mentioned_protocol== "Yes" & type_of_study_protocol =="1"~"Non-inferiority trial (explicited)",
      study_type_mentioned_protocol== "No" & type_of_study_protocol=="1"~"Non-inferiority trial (assumed)",
      study_type_mentioned_protocol== "Yes" & type_of_study_protocol=="0"~"Superiority trial (explicited)",
      study_type_mentioned_protocol== "No" & type_of_study_protocol=="0"~"Superiority trial (assumed)")) %>% 
  mutate(
    type_of_study_sup_non_inf_protocol = factor(type_of_study_sup_non_inf_protocol,
      levels = c("Superiority trial (explicited)","Superiority trial (assumed)", 
                 "Non-inferiority trial (explicited)", "Non-inferiority trial (assumed)",
                 "Equivalence trial (explicited)", "Equivalence trial (assumed)", "Not mentioned"),
      labels = c("Superiority trial (explicited)","Superiority trial (assumed)", 
                 "Non-inferiority trial (explicited)", "Non-inferiority trial (assumed)",
                 "Equivalence trial (explicited)", "Equivalence trial (assumed)", "Not mentioned")),
    cci_1st_ep_protocol = factor(cci_1st_ep_in_protocol,
                                    levels = c("0", "1", "nm"),
                                    labels = c("No", "Yes", "Not mentioned"))) %>% 
  select(-cci_1st_ep_in_protocol) %>%
  mutate(
    cci_2nd_ep_protocol= factor(cci_2nd_ep_in_protocol,
                                levels = c("0", "1", "nm"),
                                labels = c("No", "Yes", "Not mentioned"))) %>% 
  select(-cci_2nd_ep_in_protocol) %>%
  mutate(
    cci_expl_ep_protocol= factor(cci_exploratory_ep_in_protocol,
                                 levels = c("0", "1", "nm"),
                                 labels = c("No", "Yes", "Not mentioned"))) %>% 
  select(-cci_exploratory_ep_in_protocol) %>%
  mutate(
    cci_ep_protocol = case_when(
      protocol_published=="Neither protocol nor registry" ~ NA_character_,
      (cci_1st_ep_protocol %in% c("No", "nm")|is.na(cci_1st_ep_protocol)) & 
                                         (cci_2nd_ep_protocol %in% c("No", "Not mentioned")|is.na(cci_2nd_ep_protocol)) &
                                        (cci_expl_ep_protocol %in% c("No", "Not mentioned")|is.na(cci_expl_ep_protocol))~ "CCI not an endpoint",
                                       cci_1st_ep_protocol =="No" & is.na(cci_2nd_ep_protocol) &
                                         cci_expl_ep_protocol == "No" ~ "CCI not an endpoint",
                                       cci_1st_ep_protocol == "Yes" & cci_2nd_ep_protocol %in% c("No", "Not mentioned") ~ "CCI primary endpoint",
                                       cci_1st_ep_protocol == "Yes" & is.na(cci_2nd_ep_protocol) ~ "CCI primary endpoint",
                                       cci_1st_ep_protocol == "No" & cci_2nd_ep_protocol == "Yes" ~ "CCI secondary endpoint",
                                       cci_1st_ep_protocol == "Yes" & cci_2nd_ep_protocol == "Yes" ~ "CCI both primary and secondary endpoint",
                                       cci_expl_ep_protocol == "Yes" ~ "CCI exploratory endpoint")) %>% 
  mutate(
    cci_ep_protocol = factor(cci_ep_protocol,
                             levels = c("CCI primary endpoint", "CCI both primary and secondary endpoint",
                                        "CCI secondary endpoint",
                                        "CCI exploratory endpoint", "CCI not an endpoint"),
                             labels = c("CCI primary endpoint","CCI primary endpoint", "CCI non-primary endpoint",
                                        "CCI non-primary endpoint","CCI not an endpoint")),
    cci_used_for_ss_calculation_protocol= factor(cci_in_ss_calculation_of_protocol,
                                              levels = c("0", "1"),
                                              labels = c("No", "Yes"))) %>% 
  select(-cci_in_ss_calculation_of_protocol) %>%
  mutate(
    significance_considered = case_when(
      statistically_significant_difference_for_cci =="No"  ~ NA_character_,
      statistically_significant_difference_for_cci=="Yes" &
        significance_considered == "0" ~ "0",
      significance_considered == "1" ~ "1")) %>%
  mutate(significance_considered = factor(significance_considered, 
                                     levels = c("0", "1"),
                                     labels = c("Statistical significance", "Both statistical and clinical significance")),
    conclusion_of_authors = factor(conclusion_of_authors,
                                   levels = c("0", "1", "2", "3"),
                                   labels = c("Favorable to intervention",
                                              "Neutral", "Not mentioned"," Unfavorable to intervention"))
  ) %>% 
      #arrange my order  
  select(article_id, title, publication_year, 
         #vol_nr, issue_nr, journal, 
         web_url, doi, 
         trial_nr, 
         #paper_type, 
         protocol, protocol_published_original_column,
          study_type_pilot,
         pooled_analysis, post_hoc_secondary_analysis,
         discipline, continent, sample_size,
         cci_ep,
         cci_1st_ep, cci_2nd_ep, cci_expl_ep, 
         cd_cci_reported_in_results,
         cci_modification, specification_of_modification,
         primary_outcome_significant,
         statistically_significant_difference_for_cci,
         between_group_difference_of_cci_12,
         between_group_difference_of_cci,
         mean_or_median_used,
         significance_considered,
         conclusion_of_authors,
         ss_calculation, cci_used_for_ss_calculation,
         specification_of_ss_calculation,
         study_type_mentioned, type_of_study_sup_non_inf, 
         non_inf_margin,
         definition_of_non_inf_margin,
         protocol_published, doi_protocol,
         cci_ep_protocol,
         cci_1st_ep_protocol,cci_2nd_ep_protocol, cci_expl_ep_protocol,
         ss_calculation_protocol,
         cci_used_for_ss_calculation_protocol,
         specification_of_ss_calculation_protocol,
         study_type_mentioned_protocol, type_of_study_sup_non_inf_protocol,
         non_inf_margin_protocol, definition_of_non_inf_margin_protocol,
         study_must_be_excluded,
         exclusion_reason,
         comments, comments_2)

#saving excel and csv
write_xlsx(compared_RCT_2025_10_10, "database/csv_files_R_coding/Data_RCT_compared_RCT_2025_10_10.xlsx")
write_csv(compared_RCT_2025_10_10, "database/csv_files_R_coding/Data_RCT_compared_RCT_2025_10_10.csv")


#Inclusion of publication, exclusion of protocols and pilots 
included_from_compared_RCT <- compared_RCT_2025_10_10 %>% 
  filter(study_must_be_excluded!="Yes")
#saving excel and csv
write_csv(included_from_compared_RCT, "database/csv_files_R_coding/Data_included_from_compared_RCT_2025_10_10.csv")
write_xlsx(included_from_compared_RCT, "database/csv_files_R_coding/Data_included_from_compared_RCT_2025_10_10.xlsx")


included_RCT %>% filter(!is.na(trial_nr)) %>% select(article_id, trial_nr) %>% 
  arrange(trial_nr) %>% print(n=300)
df_duplicates <- extracted_RCT %>%filter(!is.na(trial_nr)) %>% 
  group_by(trial_nr) %>%
  filter(n() > 1) %>%
  select(article_id,publication_year, protocol,trial_nr) %>% 
  arrange(trial_nr) %>% print(n=50) %>% 
  ungroup()
#----------------------------------------------
included_from_compared_RCT %>% 
  filter(protocol =="Not a protocol"&cci_1st_ep=="Yes") %>% 
  group_by(protocol,protocol_published, cci_modification,article_id) %>% 
  select(article_id, protocol, protocol_published, cci_modification, cci_1st_ep) %>%  
  summarize(n=n())
