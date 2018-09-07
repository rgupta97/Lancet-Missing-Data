# Project: Gender - Missing Data (Lancet Mini Series)
# The Lancet, 2018

# Coded by Ribhav Gupta (Stanford University)
# Email: rgupta97@stanford.edu 
# Last updated: 07/25/18 

#Purpose of File: DHS Question Analysis for Men

# Files from DHS Logs (created by Valerie Meausoone) are required
# **Note: Directory should be updated based on where txt files are saved.**

#File Outputs:
# 1. MenPhase2_080418.xls to MenPhase7_080418.xls
#    with individual words from question tags
setwd("/Users/rgupta97/Documents/Fisher Honors Program")
library(readxl)
library(ggplot2)
library(wordcloud)
library(slam)
library(stringr)
library(qdapDictionaries)

input_fileVector <- c("MenPhase7.txt","MenPhase6.txt","MenPhase5.txt","MenPhase4.txt","MenPhase3.txt","MenPhase2.txt")
for(input in 1:length(input_fileVector)){
  ###########################################################################################################
  #Reads in data & formats for analysis
  DHS_Questions_integer <- read.delim(input_fileVector[input], header = FALSE, sep = "|", quote = "")
  DHS_Questions_asked <- data.frame()
  for(i in 1:ncol(DHS_Questions_integer)){
    for(j in 1:nrow(DHS_Questions_integer)){
      DHS_Questions_asked[j,i] <- trimws(as.character(DHS_Questions_integer[j,i]), which = "both")
    }
  }
  
  #Removes all rows where question was not asked or where no question was found (blank spaces)
  for(i in nrow(DHS_Questions_asked):1){
    if(grepl("na -", DHS_Questions_asked$V2[i]) | (DHS_Questions_asked$V2[i] == "") | is.na(DHS_Questions_asked$V2[i]) | grepl("-na", DHS_Questions_asked$V2[i]) | grepl("- na", DHS_Questions_asked$V2[i]) | grepl("na-", DHS_Questions_asked$V2[i])){
      DHS_Questions_asked <- DHS_Questions_asked[-i,]
    }
  }
  
  ###########################################################################################################
  #Word Counter
  DHS_Questions_wordList <- DHS_Questions_asked
  DHS_Questions_wordList <- DHS_Questions_wordList[,-1]
  
  for(i in 1:length(DHS_Questions_wordList)){
    DHS_Questions_wordList[i] <- gsub("\\.","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("\\,","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("\\:","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("\\(","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("\\)","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("\\?","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("\\=","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("\\!","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("\\#","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("\\*","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("1","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("2","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("3","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("4","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("5","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("6","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("7","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("8","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("9","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("0","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("\\\\"," ",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("\\/"," ",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("\'","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("\"","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("\\{","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("\\}","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("\\&","",DHS_Questions_wordList[i])
    DHS_Questions_wordList[i] <- gsub("\\+","",DHS_Questions_wordList[i])
    
  }
  for(i in 1:length(DHS_Questions_wordList)){
    DHS_Questions_wordList[i] <- strsplit(as.character(DHS_Questions_wordList[i]), " ")
  }
  
  for(i in length(DHS_Questions_wordList):1){
    na_found <- FALSE
    for(j in length(DHS_Questions_wordList[[i]]):1){
        if(DHS_Questions_wordList[[i]][j] == "na"){
          na_found <- TRUE
        }
    }
    if(na_found){
      DHS_Questions_wordList[[i]] <- NULL
    }
  }
  
  DHS_Questions_wordVector <- c()
  for(i in 1:length(DHS_Questions_wordList)){
    for(j in 1:length(DHS_Questions_wordList[[i]])){
      if(!(DHS_Questions_wordList[[i]][j] == "") && !(DHS_Questions_wordList[[i]][j] == " ") && !(DHS_Questions_wordList[[i]][j] == "-") && !(nchar(as.character(DHS_Questions_wordList[[i]][j])) == 1) && !(is.element(DHS_Questions_wordList[[i]][j], preposition)) && !(is.element(DHS_Questions_wordList[[i]][j], Top25Words))){
        DHS_Questions_wordVector[length(DHS_Questions_wordVector) + 1] <- DHS_Questions_wordList[[i]][j]
      }
    }
  }
  wordCount <- as.data.frame(table(DHS_Questions_wordVector))
  colnames(wordCount) <- c("label","count")
  wordCount$freq <- wordCount$count/(sum(wordCount$count))
  
  ###########################################################################################################
  if(input == 1){
    MenPhase7_raw <- DHS_Questions_integer
    Menphase7_asked <- DHS_Questions_asked
    MenPhase7 <- wordCount
  }
  if(input == 2){
    MenPhase6_raw <- DHS_Questions_integer
    Menphase6_asked <- DHS_Questions_asked
    MenPhase6 <- wordCount
  }
  if(input == 3){
    MenPhase5_raw <- DHS_Questions_integer
    Menphase5_asked <- DHS_Questions_asked
    MenPhase5 <- wordCount
  }
  if(input == 4){
    MenPhase4_raw <- DHS_Questions_integer
    Menphase4_asked <- DHS_Questions_asked
    MenPhase4 <- wordCount
  }
  if(input == 5){
    MenPhase3_raw <- DHS_Questions_integer
    Menphase3_asked <- DHS_Questions_asked
    MenPhase3 <- wordCount
  }
  if(input == 6){
    MenPhase2_raw <- DHS_Questions_integer
    Menphase2_asked <- DHS_Questions_asked
    MenPhase2 <- wordCount
  }
  WriteXLS("MenPhase2", ExcelFileName = "MenPhase2_080418.xls")
  WriteXLS("MenPhase3", ExcelFileName = "MenPhase3_080418.xls")
  WriteXLS("MenPhase4", ExcelFileName = "MenPhase4_080418.xls")
  WriteXLS("MenPhase5", ExcelFileName = "MenPhase5_080418.xls")
  WriteXLS("MenPhase6", ExcelFileName = "MenPhase6_080418.xls")
  WriteXLS("MenPhase7", ExcelFileName = "MenPhase7_080418.xls")
}
