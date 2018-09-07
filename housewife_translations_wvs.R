# Project: Gender - Missing Data (Lancet Mini Series)
# The Lancet, 2018

# Coded by Ribhav Gupta (Stanford University)
# Email: rgupta97@stanford.edu 
# Last updated: 07/25/18 

#Purpose of File: Housewife Translation Analysis

# File "WVS Bias Question List 070118_option1.xlsx" is required for analysis
# **Note: Directory should be updated based on where excel files are saved.**

#File Outputs:
# 1. 
# 2. 

setwd("/Users/rgupta97/Documents/Fisher Honors Program")
library(readxl)
library(ggplot2)
library(wordcloud)
library(slam)
####################################################################################################################################
#Reads in data & formats for analysis
sheet_raw <- WVS6_Bias_Question_List_070118_option1 <- read_excel("WVS6 Bias Question List 070118_option1.xlsx", 
                                                                 sheet = "Housewife Translations", col_names = FALSE)

sheet_raw <- sheet_raw[,c(-1,-2,-5,-6)]
sheet_raw <- sheet_raw[-1,]

languages <- unlist(sheet_raw[,2])
countries <- unlist(sheet_raw[,1])
translation_phrases <- unlist(sheet_raw[,3])
backtranslation <- unlist(sheet_raw[,4])

for(i in 1:nrow(sheet_raw)){
  for(j in 1:ncol(sheet_raw)){
    sheet_raw[i,j] <- as.character(sheet_raw[i,j])
  }
}

sheet_raw2 <- data.frame(countries, languages, translation_phrases, backtranslation)

question_num <- c()
for(i in 1:nrow(sheet_raw2)){
  if(i <= 75){
    question_num[i] <- "v54"
  }
  if(i > 75){
    question_num[i] <- "v229"
  }
}
sheet_raw2$question_num <- question_num

colnames(sheet_raw2) <- c("countries", "languages", "translation_phrase","backtranslation","question_num")


languages_all <- c()
for(i in 1:nrow(sheet_raw2)){
  if(!(is.element(sheet_raw2$languages[i],languages_all))){
    languages_all[length(languages_all) + 1] <- as.character(sheet_raw2$languages[i])
  }
}

countries_all <- c()
for(i in 1:nrow(sheet_raw2)){
  if(!(is.element(sheet_raw2$countries[i],countries_all))){
    countries_all[length(countries_all) + 1] <- as.character(sheet_raw2$countries[i])
  }
}
####################################################################################################################################

sheet_raw2_no_na <- sheet_raw2

for(i in nrow(sheet_raw2_no_na):1){
  if(is.na(sheet_raw2_no_na$backtranslation[i])){
    sheet_raw2_no_na <- sheet_raw2_no_na[-i,]
  }
}

#List of All Languages Translated
languages_all_translated <- c()
for(i in 1:nrow(sheet_raw2_no_na)){
  if(!(is.element(sheet_raw2_no_na$languages[i],languages_all_translated))){
    languages_all_translated[length(languages_all_translated) + 1] <- as.character(sheet_raw2_no_na$languages[i])
  }
}

countries_all_translated <- c()
for(i in 1:nrow(sheet_raw2_no_na)){
  if(!(is.element(sheet_raw2_no_na$countries[i],countries_all_translated))){
    countries_all_translated[length(countries_all_translated) + 1] <- as.character(sheet_raw2_no_na$countries[i])
  }
}