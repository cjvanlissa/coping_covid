library(worcs)
library(haven)
library(psych)
library(pastecs)
library(dplyr)
library(tidySEM)
source("scales_list.R")

data <- haven::read_sav("DATA _COVID_ALL - Final_manuscript_ANON.sav")
data <- as.data.frame(data)
data$date <- as.Date(data$StartDate)
data[grep("Date", names(data))] <- NULL

data$Diagnosis <- apply(data[grep("^Diagn_[a-z]+$", names(data), value = T, perl = T)], 1, function(x){
  #x = data[415, grep("^Diagn_[a-z]+$", names(data), value = T, perl = T)]
  if(all(is.na(x))){
    NA
  } else {
    if(all(unlist(x) == 0)){
      1
    } else {
      max(which(!unlist(x) == 0))
    }
    
  }
})

data[grep("^Diagn_[a-z]+$", names(data), value = T, perl = T)]<- NULL

data[c("Status", "Progress", "Duration__in_seconds_", "Finished", "DistributionChannel", "CountryHome", "HoardReas9_TEXT", "Concerned_Reason_Text", "Freq_Other_Text", "HoardReas8", "CovTest_Outcome", "Sit_Occup", "Sit_General", "CovTest_YesNo", "Diagn_Local_SUM")] <- NULL

merge_later <- c("CountryRes_YesNo", "CovTest_Outcome", "Sit_General", "CovTest_YesNo")
data[grep("^Q1\\.4_[1234]$", names(data))] <- NULL

data[sapply(data, class) == "haven_labelled"] <- lapply(data[sapply(data, class) == "haven_labelled"], haven::as_factor)

data[sapply(data, class) == "character"] <- lapply(data[sapply(data, class) == "character"], factor)

is_int <- sapply(data, class) == "numeric" & sapply(data, function(x){all(x == as.integer(x), na.rm = T)})

data[is_int] <- lapply(data[is_int], as.integer)

# data$CovTest_YesNo[which(data$CovTest_YesNo == "I'd rather not say/I don't know the outcome")] <- NA

data <- data[c("date", names(data)[!names(data) == "date"])]
# tmp <- worcs::descriptives(data)
# many_missings <- tmp$missing > .5
# synth <- worcs::synthetic(data[!many_missings])
# 
# syn_col <- which(!many_missings)
set.seed(655)
data <- data[sample(1:nrow(data)), ]

is_miss <- is.na(data)
for(i in 1:nrow(data)){
  this_col <- sample(1:ncol(data), size = (rbinom(1, round(.05*ncol(data)), .2)+1))
  data[i, this_col] <- NA
}
#set.seed(435)
#imp <- missRanger::missRanger(data)
#saveRDS(imp, "imp.RData")
imp <- readRDS("imp.RData")
imp$CountryRes_USState[!imp$CountryRes == "United States of America"] <- NA

miss_num <- rowSums(is_miss)
for(i in 1:nrow(imp)){
  #i=1
  imp[i, sample(1:ncol(imp), size = miss_num[i])] <- NA
}

# 

# 
# df <- imp
# 
# for(i in 1:ncol(df)){
#   df[[i]][is_miss[, i]] <- NA
# }

# for(i in 1:ncol(data)){
#   #i <- 2
#   orig <- data[[i]]
#   vals <- unique(sort(orig))
#   names(vals) <- vals[c(length(vals), 1:(length(vals)-1))]
#   non_missing <- which(!is.na(orig))
#   rep_these <- sample(non_missing, size = round(.01*nrow(data)))
#   data[[i]][rep_these] <- vals[as.character(orig[rep_these])]
# }

haven::write_sav(imp, "coping_with_covid.sav")

tmp <- haven::read_sav("coping_with_covid.sav")