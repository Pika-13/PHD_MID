#create new table with rows for both protocols and papers
#import included_RCT
included_RCT <- read_csv("database/included_RCT.csv")

#Tidied_table
Tidied_included_RCT <- included_RCT %>%
   pivot_longer(cols = c("study_type_mentioned_protocol", "type_of_study_protocol",
                         "cci_ep_protocol","ss_calculation_of_protocol",
                         "cci_in_ss_calculation_of_protocol",
                         "specification_of_ss_calculation_protocol",
                         "non_inf_margin_protocol" ,
                         "definition_of_non_inf_margin_protocol"),
                cols_vary = "slowest",
                names_to = "protocol",)
          
?pivot_longer()
