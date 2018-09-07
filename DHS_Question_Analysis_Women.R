# Project: Gender - Missing Data (Lancet Mini Series)
# The Lancet, 2018

# Coded by Ribhav Gupta (Stanford University)
# Email: rgupta97@stanford.edu 
# Last updated: 07/28/18 

#Purpose of File: DHS Question Analysis for Women

# Files from DHS Logs (created by Valerie Meausoone) are required
# **Note: Directory should be updated based on where txt files are saved.**

#File Outputs:
# 1. WomenPhase2_080418.xls to WomenPhase7_080418.xls
#    with individual words from question tags

setwd("/Users/rgupta97/Documents/Fisher Honors Program")
library(readxl)
library(ggplot2)
library(wordcloud)
library(slam)
library(stringr)
library(qdapDictionaries)
library(progress)
library(WriteXLS)

input_fileVector <- c("WomenPhase7.txt","WomenPhase6.txt","WomenPhase5.txt","WomenPhase4.txt","WomenPhase3.txt","WomenPhase2.txt")
for(input in 1:length(input_fileVector)){
  ###########################################################################################################
  #Reads in data & formats for analysis
  DHS_Questions_integer <- read.delim(input_fileVector[input], header = FALSE, sep = "|", quote = "")
  DHS_Questions_asked <- data.frame()
  for(i in 1:ncol(DHS_Questions_integer)){
    for(j in 1:nrow(DHS_Questions_integer)){
      DHS_Questions_asked[j,i] <- trimws(as.character(DHS_Questions_integer[j,i]), which = "both")
      if(j == 2500) print("2500 DONE")
      if(j == 50000) print("50000 DONE")
      if(j == 100000) print("100000 DONE")
      if(j == 150000) print("150000 DONE")
      if(j == 200000) print("200000 DONE")
      if(j == 250000) print("250000 DONE")
      }
  }
  
  #Removes all rows where question was not asked or where no question was found (blank spaces)
  for(i in nrow(DHS_Questions_asked):1){
    if(grepl("na -", DHS_Questions_asked$V2[i]) | (DHS_Questions_asked$V2[i] == "") | is.na(DHS_Questions_asked$V2[i]) | grepl("-na", DHS_Questions_asked$V2[i]) | grepl("- na", DHS_Questions_asked$V2[i]) | grepl("na-", DHS_Questions_asked$V2[i])){
      DHS_Questions_asked <- DHS_Questions_asked[-i,]
    }
    if(i == 2500) print("2500 LEFT")
    if(i == 50000) print("50000 LEFT")
    if(i == 100000) print("100000 LEFT")
    if(i == 150000) print("150000 LEFT")
    if(i == 200000) print("200000 LEFT")
    if(i == 250000) print("250000 LEFT")
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
    if(i == 2500) print("2500 COMPLETE")
    if(i == 50000) print("50000 COMPLETE")
    if(i == 100000) print("100000 COMPLETE")
    if(i == 150000) print("150000 COMPLETE")
    if(i == 200000) print("200000 COMPLETE")
    if(i == 250000) print("250000 COMPLETE")
  }
  for(i in 1:length(DHS_Questions_wordList)){
    DHS_Questions_wordList[i] <- strsplit(as.character(DHS_Questions_wordList[i]), " ")
    if(i == 2500) print("2500 OVER")
    if(i == 50000) print("50000 OVER")
    if(i == 100000) print("100000 OVER")
    if(i == 150000) print("150000 OVER")
    if(i == 200000) print("200000 OVER")
    if(i == 250000) print("250000 OVER")
  }
  
  for(i in length(DHS_Questions_wordList):1){
    if(identical(DHS_Questions_wordList[[i]],character(0))){
      DHS_Questions_wordList[[i]] <- NULL
    }
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
      if(!(DHS_Questions_wordList[[i]][j] == "") && !(DHS_Questions_wordList[[i]][j] == " ") && !(DHS_Questions_wordList[[i]][j] == "-") && !(nchar(as.character(DHS_Questions_wordList[[i]][j])) == 1) && !(is.element(DHS_Questions_wordList[[i]][j], preposition)) && !(is.element(DHS_Questions_wordList[[i]][j], Top100Words))){
        DHS_Questions_wordVector[length(DHS_Questions_wordVector) + 1] <- DHS_Questions_wordList[[i]][j]
      }
    }
    if(i == 2500) print("2500 WRAPPED UP")
    if(i == 50000) print("50000 WRAPPED UP")
    if(i == 100000) print("100000 WRAPPED UP")
    if(i == 150000) print("150000 WRAPPED UP")
    if(i == 200000) print("200000 WRAPPED UP")
    if(i == 250000) print("250000 WRAPPED UP")
  }
  wordCount <- as.data.frame(table(DHS_Questions_wordVector))
  colnames(wordCount) <- c("label","count")
  wordCount$freq <- wordCount$count/(sum(wordCount$count))
  
  ###########################################################################################################
  if(input == 1){
    WomenPhase7_raw <- DHS_Questions_integer
    Womenphase7_asked <- DHS_Questions_asked
    WomenPhase7 <- wordCount
  }
  if(input == 2){
    WomenPhase6_raw <- DHS_Questions_integer
    Womenphase6_asked <- DHS_Questions_asked
    WomenPhase6 <- wordCount
  }
  if(input == 3){
    WomenPhase5_raw <- DHS_Questions_integer
    Womenphase5_asked <- DHS_Questions_asked
    WomenPhase5 <- wordCount
  }
  if(input == 4){
    WomenPhase4_raw <- DHS_Questions_integer
    Womenphase4_asked <- DHS_Questions_asked
    WomenPhase4 <- wordCount
  }
  if(input == 5){
    WomenPhase3_raw <- DHS_Questions_integer
    Womenphase3_asked <- DHS_Questions_asked
    WomenPhase3 <- wordCount
  }
  if(input == 6){
    WomenPhase2_raw <- DHS_Questions_integer
    Womenphase2_asked <- DHS_Questions_asked
    WomenPhase2 <- wordCount
  }
  WriteXLS("WomenPhase2", ExcelFileName = "WomenPhase2_080418.xls")
  WriteXLS("WomenPhase3", ExcelFileName = "WomenPhase3_080418.xls")
  WriteXLS("WomenPhase4", ExcelFileName = "WomenPhase4_080418.xls")
  WriteXLS("WomenPhase5", ExcelFileName = "WomenPhase5_080418.xls")
  WriteXLS("WomenPhase6", ExcelFileName = "WomenPhase6_080418.xls")
  WriteXLS("WomenPhase7", ExcelFileName = "WomenPhase7_080418.xls")
}


