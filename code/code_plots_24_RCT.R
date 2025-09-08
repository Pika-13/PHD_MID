#project on MID with Fariba
#Load necessary packages
lapply(c("tidyverse", "data.table", "dplyr","utils", "writexl", "readxl",
         "ggpubr", "scales", "flextable", "janitor"), require,
       character.only = TRUE)
#---------------------
#reading excel file
extracted_24_RCT <- janitor::clean_names(read_xlsx("Data_extraction_24_RCT_Joana.xlsx",
                              col_names = TRUE)[-1, ])
#perform ggplot show tendency of publications accross years
library(ggplot2)
plot_tendency_across_years <- ggplot(extracted_24_RCT, aes(x = publication_year)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Number of Publications per Year",
       x = "Publication Year",
       y = "Count of Publications") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#perform ggplot show number patients accross years
plot_patients_across_years <- ggplot(extracted_24_RCT, aes(x = publication_year, y = as.numeric(sample_size))) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  labs(title = "Number of Patients per Year",
       x = "Publication Year",
       y = "Number of Patients") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# perform ggplot with 