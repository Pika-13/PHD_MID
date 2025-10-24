#mismatches df Joana and df Fariba
#Load necessary packages
lapply(c("tidyverse", "data.table","utils", "writexl", "readxl",
         "ggpubr", "scales", "flextable", "janitor", "labelled", "gtsummary", "dplyr"), require,
       character.only = TRUE)

compare_dataframes <- function(df1, df2) {
  # Check that column names and dimensions match
  if (!all(colnames(df1) == colnames(df2))) {
    stop("Column names do not match between the two datasets.")
  }
  
  if (nrow(df1) != nrow(df2)) {
    stop("Number of rows do not match between the two datasets.")
  }
  # Check if 'article_id' exists
  if (!"article_id" %in% colnames(df1)) {
    stop("'article_id' column not found in the data frames.")
  }
  # Initialize empty list to collect mismatches
  mismatches <- list()
  
  for (i in 1:nrow(df1)) {
    for (j in 1:ncol(df1)) {
      val1 <- df1[i, j]
      val2 <- df2[i, j]
      
      # Use identical to also catch NA mismatches
      if (!identical(val1, val2)) {
        mismatches[[length(mismatches) + 1]] <- data.frame(
          row = i,
          article_id = df1[i, "article_id"],  # or df1[i, article_id_column] if dynamic
          column = colnames(df1)[j],
          df1_value = as.character(val1),
          df2_value = as.character(val2),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  # Combine mismatch list into data frame
  if (length(mismatches) > 0) {
    result <- do.call(rbind, mismatches)
    return(result)
  } else {
    message("No mismatches found.")
    return(NULL)
  }
}
# Example usage:
dfFA <- janitor::clean_names(
  read_xlsx("database/Revised_Data_original_Fariba_2025_10_12_compared.xlsx",col_names = TRUE)[-1, ]) 
dfJRR <- janitor::clean_names(
  read_xlsx("database/Revised_Data_original_Joana_2025_10_12_compared.xlsx", col_names = TRUE)[-1, ]) 

#columns of interest
interest_col <- c("article_id", "title", 
                 # "cd_cci_reported_in_results",
                  "primary_outcome_significant",
                  "statistically_significant_difference_for_cci"  ,
                  "between_group_difference_of_cci" , 
                  "mean_or_median_used", 
                  "cci_used_for_ss_calculation_in_paper",                              
                  "specification_of_ss_calculation_in_paper",                          
                  "study_type_mentioned",
                  "type_of_study" ,                                                    
                  "definition_of_non_inf_margin",
                  "study_must_be_excluded_e_g_protocol_whose_study_has_been_conducted",
                  "doi_protocol","publication_year_protocol" ,
                  "cci_in_ss_calculation_of_protocol" ,
                  "specification_of_ss_calculation",                          
                  "study_type_mentioned_protocol",
                  "type_of_study_protocol" ,                                                    
                  "definition_of_non_inf_margin_protocol"
)

control_12_october_FA_jrr <- compare_dataframes(
  dfFA[interest_col], 
 # compared_RCT[interest_col], 
  dfJRR[interest_col]) %>% 
 left_join(
   dfFA %>% select(article_id), 
    by = c("article_id" = "article_id")
  )
write_xlsx(control_12_october_FA_jrr, "database/csv_files_R_coding/control_12_october_FA_jrr.xlsx")

