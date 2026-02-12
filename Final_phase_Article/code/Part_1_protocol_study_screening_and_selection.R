#This project is on the clinical purpose of research 
#It will be led by Joana Rodrigues Ribeiro and Fariba Abbassi
#And will be supervised by Milo A Puhan and Pierre Alain Clavien
#Will be submitted to Annals of Surgery,
#deadline article: February, 11th
#deadline data analysis: January (27th)
#deadline data extraction: January 25th
#deadline screening + selection: January 21st
#-----------------------------------------------------
#Author: Joana RR
#Starting date: December 19, 2025
#Last update date: January , 2025

#Protocol - sample size calculation based on 4/5 criteria (revised by MP):
#1.	Specification of the aim of the trial
#and/or
#3.	Specification of the aim of the study design (stats methods, sample size)
#AND all the following
#2.	Specification of the aim of a primary endpoint,
#4.	Use of a MID or clinically relevant effect to base sample size calculations upon
#5.	Consideration of the clinical relevance of results, e.g. using a MID (or terms associated with “clinical” or “patient” to discuss results for primary and/or secondary endpoints)

#1 and/or 3 = 100% (87/87)
#2 100%
#4 studies with SS calculation with clinical relevant DELTA: 8/87 = 9.5%
#doi: 10.1097/SLA.0000000000006232  10.1007/s00586-023-07998-6, 
#   10.1097/SLA.0000000000006055    10.1097/SLA.0000000000001474
#   10.1016/j.bja.2020.06.020       10.1056/NEJMoa2100826
#   10.1001/jamasurg.2019.5474      10.1001/jamasurg.2021.1555

#5 studies with results discussing clinical relevance: 6/87 = 7.1%
#   10.1016/S2468-1253(24)00307-8   
#   10.1200/JCO.23.01019
#   10.1016/j.bja.2020.06.020
#   10.1007/s00384-025-04939-1   
#   10.1007/s00586-023-07998-6  
#   10.1097/SLA.0000000000006055

#total 4/5 criteria =4/87 = 4.6%
#   10.1097/SLA.0000000000006055
#   10.1016/j.bja.2020.06.020 
#   10.1007/s00586-023-07998-6  
#   10.1097/SLA.0000000000006055

#install packages if not installed
list.of.packages <- c("tibble" ,"tidyverse", "dplyr", "data.table", "colorspace", "readxl", "haven",
                      "ggpubr", "ggbeeswarm", "scales", "gtsummary", 
                      "data.table", "flextable", "countrycode", "presize")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#loading packages from library
lapply(c("tibble" ,"tidyverse", "dplyr", "data.table", "colorspace", "readxl", "haven",
         "ggpubr", "ggbeeswarm", "scales", "gtsummary", 
         "data.table", "flextable", "countrycode", "presize"), 
       require,character.only = TRUE)

#sample size in January 14th
prec_prop(n=NULL, 0.05, conf.width = 0.07) # n=161
#1612 led to 87


#Screening started in January 16th
#First agreement test with cohen's k
agreement_after_50 <- matrix(
  c( 14, 3, 33,
    14, 4, 32), nrow=2,
  dimnames=list(
              c("Rater JP","Rater JRR"),c("Include", "Maybe", "Exclude")),
  byrow=TRUE)
#18 conflicts/50 = 36%

#cohen
library(psych)        #if not: install.packages("psych")
cohen.kappa(agreement_after_50)

#CI interval
pobsCI <- wilson(x=sum(diag(agreement_after_50)), n=sum(agreement_after_50))
pexp <- sum(diag(chisq.test(agreement_after_50)$expected))/sum(agreement_after_50) ## substitution method
round((pobsCI-pexp)/(1-pexp), 2)
#CI interval bootstrap
res <- biostatUZH::confIntKappa(agreement_after_50, type="Cohen", m=10000)
round(res$boot.quant, 2)


#Second agreement test with cohen's k
agreement_after_51 <- matrix(
  c( 1 , , 3,
     1 , 0, 3 ), nrow=2,
  dimnames=list(
    c("Rater JP","Rater JRR"),c("Include", "Maybe", "Exclude")),
  byrow=TRUE)
# conflicts/51 = 4
