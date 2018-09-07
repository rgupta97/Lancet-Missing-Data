# Project: Gender - Missing Data (Lancet Mini Series)
# The Lancet, 2018

# Coded by Ribhav Gupta (Stanford University)
# Email: rgupta97@stanford.edu 
# Last updated: 07/21/18 

#Purpose of File: Tag Frequency Analysis for WVS

# File "WVS Bias Question List 070118_option1.xlsx" is required for analysis
# **Note: You will need to change your directory files to load these excels.**

#File Outputs:
# 1. Barplot of frequency that gender-biased questions (across 4 possible types of bias) fall into specific tag categories
# 2. Wordcloud for most common categories with gender-biased questions
# 3. Barplot of frequency of gender-biased questions in pairs of categories 

setwd("/Users/rgupta97/Documents/Fisher Honors Program")
library(readxl)
library(ggplot2)
library(wordcloud)
library(slam)
####################################################################################################################################
## Reading in spreadsheet and formatting it to include question number and tags only
wvs_rawData <- read_excel("~/Documents/Fisher Honors Program/WVS6 Bias Question List 070118_option1.xlsx", col_names = FALSE)
header <- c()


# Removes Template Columns
wvs_rawData <- wvs_rawData[,-1:-4]

#Populates headers
for(i in seq(1,ncol(wvs_rawData),4)){
  header[length(header) + 1] <- wvs_rawData[1,i]
}

wvs_rawData <- wvs_rawData[-1:-2,]

# Removes Empty Columns
for(i in rev(seq(4,ncol(wvs_rawData),4))){
  wvs_rawData <- wvs_rawData[,-i]
}

# Removes Question Columns
for(i in rev(seq(2,ncol(wvs_rawData),3))){
  wvs_rawData <- wvs_rawData[,-i]
}

endpoints <- c(1)
## Formats Matrix
# Aggregates Everything Into Two Columns
for(i in 2:(ncol(wvs_rawData)/2)){
  index_na <- which(is.na(wvs_rawData[,1]))[1]
  endpoints[length(endpoints) + 1] <- index_na
  index_add <- which(is.na(wvs_rawData[,i*2]))[1]
  if(is.na(index_add)){
    index_add <- nrow(wvs_rawData[,i*2])
  }
  for(j in 1:index_add){
    wvs_rawData[index_na + j - 1,1] <- wvs_rawData[j,(i*2) - 1]
    wvs_rawData[index_na + j - 1,2] <- wvs_rawData[j,(i*2)]
  }
}

#Removes Extra Columns
wvs_rawData <- wvs_rawData[,-3:-ncol(wvs_rawData)]

#Removes All Extra Rows
if(!is.na(which(is.na(wvs_rawData[,1]))[1])){
  wvs_rawData <- wvs_rawData[-which(is.na(wvs_rawData[,1])),]
}

endpoints[length(endpoints) + 1] <- (nrow(wvs_rawData) + 1)

for(i in 1:(length(endpoints) - 1)){
  temp_frame <- wvs_rawData[endpoints[i]:(endpoints[i+1]),]
  temp_frame <- temp_frame[-nrow(temp_frame),]
  if(i == 1){
    wvs_gender_compare <- temp_frame
  }
  if(i == 2){
    wvs_gender_men <- temp_frame
  }
  if(i == 3){
    wvs_gender_women <- temp_frame
  }
  if(i == 4){
    wvs_gender_neutral <- temp_frame
  }
}
  

####################################################################################################################################
## Creates framework for counting number of times tag is found in raw data

tags_raw <- WVS6_Bias_Question_List_070118_option1 <- read_excel("WVS6 Bias Question List 070118_option1.xlsx", 
                                                     sheet = "Master Tag List", col_names = FALSE)
tags <- c()
for(i in 2:nrow(tags_raw)){
  tags[i-1] <- tags_raw[i,1]
}
tags <- unlist(tags)

tag_count <- c(0)
for(i in 1:(length(tags) - 1)){
  tag_count <- append(tag_count,0, after = length(tag_count))
}

wvs_counter <- data.frame(tags,tag_count)
wvs_counter_compare <- wvs_counter
wvs_counter_men <- wvs_counter
wvs_counter_women <- wvs_counter
wvs_counter_neutral <- wvs_counter

####################################################################################################################################
## Counts number of times tag is found throughout the raw data

#Provides counts for all categories
wvs_all <- list(wvs_gender_compare,wvs_gender_men,wvs_gender_women,wvs_gender_neutral)
wvs_counter_all <- list(wvs_counter_compare,wvs_counter_men,wvs_counter_women, wvs_counter_neutral)
for(k in 1:length(wvs_all)){
  for(i in 1:nrow(wvs_all[[k]])){
    for(j in 1:length(tags)){
      if(grepl(tags[j],wvs_all[[k]][i,2])){
        wvs_counter_all[[k]][j,2] <- wvs_counter_all[[k]][j,2] + 1
      }
    }
  }
  if(k == 1){
    wvs_counter_compare <- wvs_counter_all[[k]]
  }
  if(k == 2){
    wvs_counter_men <- wvs_counter_all[[k]]
  }
  if(k == 3){
    wvs_counter_women <- wvs_counter_all[[k]]
  }
  if(k == 4){
    wvs_counter_neutral <- wvs_counter_all[[k]]
  }
}

#Only includes categories with >0 values
for(j in 1:length(header)){
  if(j == 1){
    wvs_counter_temp <- wvs_counter_compare
  }
  if(j == 2){
    wvs_counter_temp <- wvs_counter_men
  }
  if(j == 3){
    wvs_counter_temp <- wvs_counter_women
  }
  if(j == 4){
    wvs_counter_temp <- wvs_counter_neutral
  }
  irrelevantCategories <- c()
  for(i in 1:nrow(wvs_counter_temp)){
    if(wvs_counter_temp[i,2] == 0){
      irrelevantCategories <- append(irrelevantCategories,i, after = length(irrelevantCategories))
    }
  }
  if(j == 1){
    wvs_counter_compare <- wvs_counter_temp[-irrelevantCategories,]
  }
  if(j == 2){
    wvs_counter_men <- wvs_counter_temp[-irrelevantCategories,]
  }
  if(j == 3){
    wvs_counter_women <- wvs_counter_temp[-irrelevantCategories,]
  }
  if(j == 4){
    wvs_counter_neutral <- wvs_counter_temp[-irrelevantCategories,]
  }
}


#Adds in Count Freq.
for(j in 1:length(header)){
  
  tag_freq <- c()
  if(j == 1){
    wvs_counter_temp <- wvs_counter_compare
    wvs_data_temp <- wvs_gender_compare
  }
  if(j == 2){
    wvs_counter_temp <- wvs_counter_men
    wvs_data_temp <- wvs_gender_men
  }
  if(j == 3){
    wvs_counter_temp <- wvs_counter_women
    wvs_data_temp <- wvs_gender_women
  }
  if(j == 4){
    wvs_counter_temp <- wvs_counter_neutral
    wvs_data_temp <- wvs_gender_neutral
  }
  
  num_question <- nrow(wvs_data_temp)
  if(num_question > 0){
    for(i in 1:nrow(wvs_counter_temp)){
      tag_freq[i] <- 100*(wvs_counter_temp$tag_count[i]/num_question)
    }
  }
  wvs_counter_temp$tag_freq <- tag_freq
  if(j == 1){
    wvs_counter_compare <- wvs_counter_temp
  }
  if(j == 2){
    wvs_counter_men <- wvs_counter_temp
  }
  if(j == 3){
    wvs_counter_women <- wvs_counter_temp
  }
  if(j == 4){
    wvs_counter_neutral <- wvs_counter_temp
  }
}


#Rearranges in Descending Order
wvs_counter_compare <- wvs_counter_compare[order(wvs_counter_compare$tag_count, decreasing = TRUE),]
wvs_counter_men <- wvs_counter_men[order(wvs_counter_men$tag_count, decreasing = TRUE),]
wvs_counter_women <- wvs_counter_women[order(wvs_counter_women$tag_count, decreasing = TRUE),]
wvs_counter_neutral <- wvs_counter_neutral[order(wvs_counter_neutral$tag_count, decreasing = TRUE),]



####################################################################################################################################
## Repeated for two tag frequency
for(r in 1:length(header)){
  two_category_tags <- c()
  if(r == 1){
    wvs_counter_temp <- wvs_counter_compare
    wvs_data_temp <- wvs_gender_compare
  }
  if(r == 2){
    wvs_counter_temp <- wvs_counter_men
    wvs_data_temp <- wvs_gender_men
  }
  if(r == 3){
    wvs_counter_temp <- wvs_counter_women
    wvs_data_temp <- wvs_gender_women
  }
  if(r == 4){
    wvs_counter_temp <- wvs_counter_neutral
    wvs_data_temp <- wvs_gender_neutral
  }
  if(nrow(wvs_counter_temp) > 0){
    for(i in 1:(length(tags) - 1)){
      for(j in 1:(length(tags) - i)){
        two_category_tags[length(two_category_tags) + 1] <- paste(tags[i],tags[i+j], sep = " & ")
      }
    }
    two_category_count <- c(0)
    for(i in 1:(length(two_category_tags) - 1)){
      two_category_count <- append(two_category_count,0, after = length(two_category_count))
    }
    for(i in 1:nrow(wvs_data_temp)){
      for(j in 1:(length(tags) - 1)){
        for(k in 1:(length(tags) - j)){
          if(grepl(tags[j],wvs_data_temp[i,2]) && grepl(tags[j+k],wvs_data_temp[i,2])){
            n <- (length(tags))-1
            m <- (n - j) + 1
            two_category_count[(n*(n+1)/2) - (m*(m+1)/2) + k] <- two_category_count[(n*(n+1)/2) - (m*(m+1)/2) + k] + 1
          }
        }
      }
    }
  }
  if(nrow(wvs_counter_temp) == 0){
    two_category_count <- c(0)
    two_category_tags <- c(0)
  }
  for(i in length(two_category_count):1){
    if(two_category_count[i] == 0){
      two_category_count <- two_category_count[-i]
      two_category_tags<- two_category_tags[-i]
    }
  }
  two_category_frame <- data.frame(two_category_tags,two_category_count)
  two_category_frame <- two_category_frame[order(two_category_frame$two_category_count, decreasing = TRUE),]
  if(r == 1){
    two_category_compare <- two_category_frame
  }
  if(r == 2){
    two_category_men <- two_category_frame
  }
  if(r == 3){
    two_category_women <- two_category_frame
  }
  if(r == 4){
    two_category_neutral <- two_category_frame
  }
}


####################################################################################################################################
## NEED TO UPDATE ALL
## Graph Options

#Bar Plot 1 (1-Tag Frequency)

#Reorders Levels of factor to be based on frequency rather than alphabetical

wvs_counter_compare$tags <- factor(wvs_counter_compare$tags, 
                                   levels = wvs_counter_compare$tags[order(wvs_counter_compare$tag_count, decreasing = FALSE)])
wvs_counter_men$tags <- factor(wvs_counter_men$tags, 
                                   levels = wvs_counter_men$tags[order(wvs_counter_men$tag_count, decreasing = FALSE)])
wvs_counter_women$tags <- factor(wvs_counter_women$tags, 
                               levels = wvs_counter_women$tags[order(wvs_counter_women$tag_count, decreasing = FALSE)])
wvs_counter_neutral$tags <- factor(wvs_counter_neutral$tags, 
                               levels = wvs_counter_neutral$tags[order(wvs_counter_neutral$tag_count, decreasing = FALSE)])

#plots barplot (counts)
p <-ggplot(data=wvs_counter_compare, aes(x=tags, y=tag_count)) +
  geom_bar(stat="identity")
p + coord_flip() + labs(title = toString(header[1]), y = "Frequency", x = "Tag") + theme(plot.title = element_text(size = 20, hjust = 1), axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 25))

p <-ggplot(data=wvs_counter_men, aes(x=tags, y=tag_count)) +
  geom_bar(stat="identity")
p + coord_flip() + labs(title = toString(header[2]), y = "Frequency", x = "Tag") + theme(plot.title = element_text(size = 20, hjust = 1), axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 25))

p <-ggplot(data=wvs_counter_women, aes(x=tags, y=tag_count)) +
  geom_bar(stat="identity")
p + coord_flip() + labs(title = toString(header[3]), y = "Frequency", x = "Tag") + theme(plot.title = element_text(size = 20, hjust = 1), axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 25))

p <-ggplot(data=wvs_counter_neutral, aes(x=tags, y=tag_count)) +
  geom_bar(stat="identity")
p + coord_flip() + labs(title = toString(header[4]), y = "Frequency", x = "Tag") + theme(plot.title = element_text(size = 20, hjust = 1), axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 25))

#plots barplot (frequency)
p <-ggplot(data=wvs_counter_compare, aes(x=tags, y=tag_freq)) +
  geom_bar(stat="identity")
p + coord_flip() + labs(title = toString(header[1]), y = "% Questions", x = "Tag") + theme(plot.title = element_text(size = 20, hjust = 1), axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 25))

p <-ggplot(data=wvs_counter_men, aes(x=tags, y=tag_freq)) +
  geom_bar(stat="identity")
p + coord_flip() + labs(title = toString(header[2]), y = "% Questions", x = "Tag") + theme(plot.title = element_text(size = 20, hjust = 1), axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 25))

p <-ggplot(data=wvs_counter_women, aes(x=tags, y=tag_freq)) +
  geom_bar(stat="identity")
p + coord_flip() + labs(title = toString(header[3]), y = "% Questions", x = "Tag") + theme(plot.title = element_text(size = 20, hjust = 1), axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 25))

p <-ggplot(data=wvs_counter_neutral, aes(x=tags, y=tag_freq)) +
  geom_bar(stat="identity")
p + coord_flip() + labs(title = toString(header[4]), y = "% Questions", x = "Tag") + theme(plot.title = element_text(size = 20, hjust = 1), axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 25))

# Word Cloud

wordcloud(wvs_counter_compare[,1],wvs_counter_compare[,2], random.color = FALSE, min.freq = 1, max.words = 10, scale = c(5,0.5))
wordcloud(wvs_counter_men[,1],wvs_counter_men[,2], random.color = FALSE, min.freq = 1, max.words = 10, scale = c(5,0.5))
wordcloud(wvs_counter_women[,1],wvs_counter_women[,2], random.color = FALSE, min.freq = 1, max.words = 10, scale = c(2.7,0.5))
wordcloud(wvs_counter_neutral[,1],wvs_counter_neutral[,2], random.color = FALSE, min.freq = 1, max.words = 10, scale = c(3.3,0.5))

# Bar Plot 2 (2-Tag Frequency)

#Reorders Levels of factor to be based on frequency rather than alphabetical

two_category_compare$two_category_tags <- factor(two_category_compare$two_category_tags, 
                                   levels = two_category_compare$two_category_tags[order(two_category_compare$two_category_count, decreasing = FALSE)])
two_category_men$two_category_tags <- factor(two_category_men$two_category_tags, 
                                                 levels = two_category_men$two_category_tags[order(two_category_men$two_category_count, decreasing = FALSE)])
two_category_women$two_category_tags <- factor(two_category_women$two_category_tags, 
                                             levels = two_category_women$two_category_tags[order(two_category_women$two_category_count, decreasing = FALSE)])
two_category_neutral$two_category_tags <- factor(two_category_neutral$two_category_tags, 
                                               levels = two_category_neutral$two_category_tags[order(two_category_neutral$two_category_count, decreasing = FALSE)])

q <-ggplot(two_category_compare, aes(x=two_category_tags, y=two_category_count)) +
  geom_bar(stat="identity")
q + coord_flip() + labs(title = toString(header[1]), y = "Frequency", x = "Tag") + theme(plot.title = element_text(size = 20, hjust = 1), axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), axis.text.x = element_text(size = 17), axis.text.y = element_text(size = 20))

q <-ggplot(two_category_men, aes(x=two_category_tags, y=two_category_count)) +
  geom_bar(stat="identity")
q + coord_flip() + labs(title = toString(header[2]), y = "Frequency", x = "Tag") + theme(plot.title = element_text(size = 20, hjust = 1), axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), axis.text.x = element_text(size = 17), axis.text.y = element_text(size = 20))

q <-ggplot(two_category_women, aes(x=two_category_tags, y=two_category_count)) +
  geom_bar(stat="identity")
q + coord_flip() + labs(title = toString(header[3]), y = "Frequency", x = "Tag") + theme(plot.title = element_text(size = 20, hjust = 1), axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), axis.text.x = element_text(size = 17), axis.text.y = element_text(size = 20))

q <-ggplot(two_category_neutral, aes(x=two_category_tags, y=two_category_count)) +
  geom_bar(stat="identity")
q + coord_flip() + labs(title = toString(header[4]), y = "Frequency", x = "Tag") + theme(plot.title = element_text(size = 20, hjust = 1), axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), axis.text.x = element_text(size = 17), axis.text.y = element_text(size = 16))
