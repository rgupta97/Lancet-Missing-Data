# Project: Gender - Missing Data (Lancet Mini Series)
# The Lancet, 2018

# Coded by Ribhav Gupta (Stanford University)
# Email: rgupta97@stanford.edu 
# Last updated: 08/16/18 

#Purpose of File: Shortens word list to include only word stems

# Files from DHS Logs (created by Valerie Meausoone) are required
# **Note: Directory should be updated based on where txt files are saved.**

#File Outputs:
# 1. 
# 2. 
setwd("/Users/rgupta97/Documents/Fisher Honors Program")
library(readxl)
library(ggplot2)
library(wordcloud)
library(slam)
library(stringr)
library(qdapDictionaries)
library(quanteda)
library(WriteXLS)

########################################################################

MenPhase7 <- read_excel("MenPhase7_080318.xls")
WomenPhase7 <- read_excel("WomenPhase7_080318.xls")
MenPhase4 <- read_excel("MenPhase4_080318.xls")
WomenPhase4 <- read_excel("WomenPhase4_080318.xls")
MenPhase2 <- read_excel("MenPhase2_080418.xls")
WomenPhase2 <- read_excel("WomenPhase2_080418.xls")

numSurveys <- 6
for(survey in 1:numSurveys){
  if(survey == 1){
    gender_phase <- WomenPhase7
  }
  if(survey == 2){
    gender_phase <- MenPhase7
  }
  if(survey == 3){
    gender_phase <- WomenPhase4
  }
  if(survey == 4){
    gender_phase <- MenPhase4
  }
  if(survey == 5){
    gender_phase <- WomenPhase2
  }
  if(survey == 6){
    gender_phase <- MenPhase2
  }
  word_list <- gender_phase$label
  word_list_token <- tokens(word_list)
  word_list_stems_token <- tokens_wordstem(word_list_token)
  word_list_stems_unique <- unique(as.character(word_list_stems_token))
  word_list_stems_count <- c()
  temp_frame <- data.frame(as.character(word_list_stems_token),gender_phase$count)
  colnames(temp_frame) <- c("word","count")
  for(i in nrow(temp_frame):1){
    reused <- FALSE
    if(i > 1){
      for(j in (i - 1):1){
        if(temp_frame$word[i] == temp_frame$word[j] && !reused){
          temp_frame$count[j] <- temp_frame$count[j] + temp_frame$count[i]
          reused <- TRUE
        }
      }
      if(reused){
        temp_frame <- temp_frame[-i,]
      }
    }
  }
  
  gender_phase_frame <- temp_frame
  
  for(i in nrow(gender_phase_frame):1){
    if(is.element(gender_phase_frame$word[i], Top100Words)){
      gender_phase_frame <- gender_phase_frame[-i,]
    }
  }
  if(survey == 1){
    WomenPhase7_frame <- gender_phase_frame
  }
  if(survey == 2){
    Men_Phase7_frame <- gender_phase_frame
  }
  if(survey == 3){
    WomenPhase4_frame <- gender_phase_frame
  }
  if(survey == 4){
    Men_Phase4_frame <- gender_phase_frame
  }
  if(survey == 5){
    WomenPhase2_frame <- gender_phase_frame
  }
  if(survey == 6){
    Men_Phase2_frame <- gender_phase_frame
  }
}

WriteXLS(c("WomenPhase7_frame","WomenPhase4_frame","WomenPhase2_frame","Men_Phase7_frame","Men_Phase4_frame","Men_Phase2_frame"),ExcelFileName = "DHS_byGender_wordStems_082418.xlsx", SheetNames = c("WomenPhase7","WomenPhase4 WHO","WomenPhase2 DHS","MenPhase7","MenPhase4","MenPhase2"))

