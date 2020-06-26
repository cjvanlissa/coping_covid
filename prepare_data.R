# In this file, write the R-code necessary to load your original data file
# (e.g., an SPSS, Excel, or SAS-file), and convert it to a data.frame. Then,
# use the function open_data(your_data_frame) or closed_data(your_data_frame)
# to store the data.

library(worcs)
library(foreign)
library(psych)
library(pastecs)
library(dplyr)
library(tidySEM)
source("scales_list.R")

data <- read.spss("DATA_PsychologicalImpact_COVID19_anon.sav",
                  use.value.labels = FALSE,
                  to.data.frame = TRUE)
names(data) <- tolower(names(data))

# I’ve also included the SPSS syntax I’ve used for your information, though it follows the pre-registered procedures directly. 
# 
# Everything is looking fine, except that there were some issues of lower reliability for the brief COPE scale (it has 14 subscales, each of two items), and upon closer inspection of the items, I think this follows from the items simply not working in a COVID-19 situation (see Coping scale notes). 
# 
# However, I’m not sure what the best way of dealing with this is – for now I’ve let the subscales be represented by one item in some cases, and two items in others (where reliability was fine), but I’m not sure if this is best practice.
# 
# Let me know if you need anything else to start the analyses.

scales <- create_scales(data, scales_list, omega = "omega.tot", write_files = TRUE)

data <- cbind(data, scales$scores)

new_names <- c("severe_life" = "perceived_threat",
               "diagn_local" = "local_diagnoses",
               "diagn_no" = "global_diagnoses",
               "sit_isoldays" = "days_isolation",
               "demo_age" = "age",
               "demo_gender" = "gender",
               "demo_education" = "education",
               "demo_income" = "income",
               "virus_natart" = "conspiracy",
               "emo_confused" = "confusion",
               "fakenews" = "fake_news"
               )
names(data)[match(names(new_names), names(data))] <- new_names

use_variables <- c(
  "countryres", 
  new_names,
  names(scales_list)[names(scales_list) %in% names(data)],
  grep("^(trust|freq)_(who|nhs|gov|newspaper|fb|tw|ig|maps|google)$", names(data), value = TRUE),
  unique(unlist(scales_list)[unlist(scales_list) %in% names(data)])
)

closed_data(data[, use_variables])
