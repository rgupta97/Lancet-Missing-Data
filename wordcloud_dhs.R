# Project: Gender - Missing Data (Lancet Mini Series)
# The Lancet, 2018

# Coded by Ribhav Gupta (Stanford University)
# Email: rgupta97@stanford.edu 
# Last updated: 08/27/18 

#Purpose of File: Temporary Word Cloud for DHS Phase 7 (updated)

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

WomenPhase7_frame <- read_excel("DHS_byGender_wordStems_082718.xlsx", sheet = "WomenPhase7")
WomenPhase4_frame <- read_excel("DHS_byGender_wordStems_082718.xlsx", sheet = "WomenPhase4")
WomenPhase2_frame <- read_excel("DHS_byGender_wordStems_082718.xlsx", sheet = "WomenPhase2")
Men_Phase7_frame <- read_excel("DHS_byGender_wordStems_082718.xlsx", sheet = "MenPhase7")
Men_Phase4_frame <- read_excel("DHS_byGender_wordStems_082718.xlsx", sheet = "MenPhase4")
Men_Phase2_frame <- read_excel("DHS_byGender_wordStems_082718.xlsx", sheet = "MenPhase2")

wordcloud(WomenPhase7_frame$word,WomenPhase7_frame$count, random.color = FALSE, max.words = 50, scale = c(3,0.5))
wordcloud(Men_Phase7_frame$word,Men_Phase7_frame$count, random.color = FALSE, max.words = 50, scale = c(3,0.5))
wordcloud(WomenPhase4_frame$word,WomenPhase4_frame$count, random.color = FALSE, max.words = 50, scale = c(3,0.5))
wordcloud(Men_Phase4_frame$word,Men_Phase4_frame$count, random.color = FALSE, max.words = 50, scale = c(3,0.5))
wordcloud(WomenPhase2_frame$word,WomenPhase2_frame$count, random.color = FALSE, max.words = 50, scale = c(3,0.5))
wordcloud(Men_Phase2_frame$word,Men_Phase2_frame$count, random.color = FALSE, max.words = 50, scale = c(3,0.5))
