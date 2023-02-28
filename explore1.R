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

#++ Frequent words appear---------------

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

#Example: 
wordCountPerSurahShahih <- wordCountPerSurah(quranSahihEn,surahSeq = 2, noOfWord = 1000, removeStopWords = TRUE)


wordCountPerSurahYusufAli <- wordCountPerSurah(quranYusufaliEn,surahSeq = 2, noOfWord = 1000, removeStopWords = TRUE)





#++ Plotting most frequent words---------------


wordCountBarPlot <- function(wordCountDf){
  freqBarPlot <- ggplot(wordCountDf, aes(n, listOfWord)) +
    geom_col()+
    labs(y=NULL)
  
  return(freqBarPlot)
}

#Example:
wordCountBarPlot(wordCountPerSurah(quranSahihEn, surahSeq = 1,noOfWord = 40,removeStopWords = TRUE))


#++ Word frequencies proportion comparison from diff translator----------

wordFrequencyProp <- wordCountPerSurahShahih %>% 
  mutate(translator = "Shahih") %>% 
  bind_rows(wordCountPerSurahYusufAli %>% mutate(translator = "YusufAli")) %>% 
  group_by(translator) %>% 
  mutate(proportion = n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = translator, values_from = proportion) %>% 
  pivot_longer(YusufAli, names_to = "Translator", values_to = "proportion")


library(scales)
ggplot(wordFrequencyProp, aes(x = proportion, y = Shahih, 
                      color = abs(Shahih - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = listOfWord), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~Translator, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Shahih", x = NULL)
  



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














