#Load library-----------

library(quRan)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(dplyr)

#wordcloud
library(wordcloud)
library(RColorBrewer)


#Load quran data----------
quran_ar <- quran_ar
qur_trans_sahih <- quran_en_sahih
qur_trans_ysf <- quran_en_yusufali


#Unnest token-------


## Sahih-----
qur_trans_sahih_tkn <- quran_en_sahih |> 
  unnest_tokens(word, text)


## Yusof-----
qur_trans_ysf_tkn <- quran_en_yusufali |> 
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

## Sahih-----
wc_qur_trans_sahih_tk <-  qur_trans_sahih_tkn |> 
  count(surah_title_en, word, sort = TRUE)

sum_wc_trans_sahis_tk <- wc_qur_trans_sahih_tk |>  
  group_by(surah_title_en) |> 
  summarize(total = sum(n))

wc_qur_trans_sahih_tk <- wc_qur_trans_sahih_tk |> 
  left_join(sum_wc_trans_sahis_tk, by = "surah_title_en") |> 
  mutate(tf = n/total)


## Yusof Ali---------

wc_qur_trans_ysf_tk <-  qur_trans_ysf_tkn |> 
  count(surah_title_en, word, sort = TRUE)

sum_wc_trans_ysf_tk <- wc_qur_trans_ysf_tk |>  
  group_by(surah_title_en) |> 
  summarize(total = sum(n))

wc_qur_trans_ysf_tk <- wc_qur_trans_ysf_tk |> 
  left_join(sum_wc_trans_ysf_tk, by = "surah_title_en") |> 
  mutate(tf = n/total)

#Plotting tf-----------

last6_surahs = c("An-Naas", "Al-Falaq", "Al-Ikhlaas", "Al-Masad",
                 "An-Nasr", "Al-Kaafiroon")


## Sahih-----
wc_qur_trans_sahih_tk |> 
  filter(surah_title_en %in% last6_surahs) |> 
  ggplot(aes(tf, fill = surah_title_en)) + 
    geom_histogram() +
    facet_wrap(~surah_title_en, ncol = 2) +
    theme_bw() + 
    ggtitle("Sahih Translation")


## Yusof ------

wc_qur_trans_ysf_tk |> 
  filter(surah_title_en %in% last6_surahs) |> 
  ggplot(aes(tf, fill = surah_title_en)) + 
  geom_histogram() +
  facet_wrap(~surah_title_en, ncol = 2) +
  theme_bw() +
  ggtitle("Yusof Ali Translation")


### Cont page 45

