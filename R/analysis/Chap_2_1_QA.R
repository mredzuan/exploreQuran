#Load library-----------

library(quRan)
library(tidyverse)
library(tidytext)
library(ggplot2)

#wordcloud
library(wordcloud)
library(RColorBrewer)


#Load quran data----------
quran_ar <- quran_ar
qur_trans_sahih <- quran_en_sahih
qur_trans_ysf <- quran_en_yusufali


#Unnest token-------

qur_trans_sahih_tkn <- quran_en_sahih |> 
  unnest_tokens(word, text)


#Word cloud------------
qur_trans_sahih_tkn |> count(word) |> 
  with(wordcloud(words=word, 
                 freq=n, 
                 max.words = 200,
                 random.order = FALSE,
                 rot.per = 0.35,
                 colors = brewer.pal(8, "Dark2")))


#Document frequency------


