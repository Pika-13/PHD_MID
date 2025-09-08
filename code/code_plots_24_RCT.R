#project on MID with Fariba
#Load necessary packages
lapply(c("tidyverse", "data.table", "dplyr","utils", "writexl", "readxl",
         "ggpubr", "scales", "flextable", "janitor"), require,
       character.only = TRUE)
#---------------------
#reading excel file
extracted_24_RCT <- janitor::clean_names(read_xlsx("Data_extraction_24_RCT_Joana.xlsx",
                              col_names = TRUE)[-1, ])

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

