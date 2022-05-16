library(quRan)
library(tidyverse)
library(tidytext)
library(dplyr)
library(ggplot2)



library(xml2)
library(XML)


#Quran translation from quRan package----------
quranSahihEn <- quran_en_sahih
quranYusufaliEn <- quran_en_yusufali


#Basic Analysis--------
#++ Tokenization--------

surahId <- c(36)

tokensQuranTextSahihEn <- quranSahihEn %>% 
  filter(surah_id == surahId) %>% 
  select(text) %>% 
  unnest_tokens(listOfWord, text) %>% 
  anti_join(stop_words, by = c("listOfWord" = "word"))



#++ Frequent words appear---------------

wordCount <- tokensQuranTextSahihEn %>% 
  count(listOfWord, sort = TRUE) %>% 
  mutate(listOfWord =  reorder(listOfWord, n))


wordCountPerSurah <- function(quranTranslationDf, surahSeq, noOfWord=15, removeStopWords=TRUE){
  if(surahSeq > 114){
    stop("Only max 114 Surah available in Quran!")
  }
    
  
  if(removeStopWords){
    tokensQuranText <- quranTranslationDf %>% 
      filter(surah_id == surahSeq) %>% 
      select(text) %>% 
      unnest_tokens(listOfWord, text) %>% 
      anti_join(stop_words, by = c("listOfWord" = "word"))
  }else{
    tokensQuranText <- quranTranslationDf %>% 
      filter(surah_id == surahSeq) %>% 
      select(text) %>% 
      unnest_tokens(listOfWord, text)
  }
  
  wordCount <- tokensQuranText %>% 
    count(listOfWord, sort = TRUE) %>% 
    mutate(listOfWord =  reorder(listOfWord, n)) %>% 
    slice_head(n=noOfWord)
  
  return(wordCount)
  
}




#++ Plotting most frequent words---------------


wordFrequencyBarPlot <- function(wordCountDf){
  freqBarPlot <- ggplot(wordCountDf, aes(n, listOfWord)) +
    geom_col()+
    labs(y=NULL)
  
  return(freqBarPlot)
}

#Example:
wordFrequencyBarPlot(wordCountPerSurah(quranSahihEn, surahSeq = 50,noOfWord = 40,removeStopWords = TRUE))


#++ Word frequencies proportion comparison from diff translator----------



#Sentiment analysis----------------
  




#Quran from https://tanzil.net/trans/---------------

testReadText <- read.delim("03_Data/testTextFile",header = FALSE, sep = "|")
read.csv("03_Data/testTextFile", header = FALSE)

quranMalayBasmeih <- read.delim("https://tanzil.net/trans/ms.basmeih", header = FALSE, sep = "|")
quranEngSahihInt <- read.delim("https://tanzil.net/trans/en.sahih", header = FALSE, sep = "|", strip.white = FALSE)


quranMalayBasmeih2 <- read_delim("https://tanzil.net/trans/ms.basmeih", delim = "|", col_names = FALSE)









#Test----------------
# Text clean-up


quranXml <- read_xml("03_Data/en.sahih_xmlformatTestEdit.xml")

quranXMLTest <- xmlParse("03_Data/en.sahih_xmlformatTestEdit.xml")


testReadByline <-  readLines("https://tanzil.net/trans/en.sahih")

testReadBylineTibble <- tibble(testReadByline)














