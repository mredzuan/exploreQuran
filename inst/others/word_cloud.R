library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(gridExtra)


#load dataset --------------
trans_malay_basmeih <- tanzil_translation("https://tanzil.net/trans/ms.basmeih")
trans_en_sahih <- tanzil_translation("https://tanzil.net/trans/en.sahih")
trans_en_yusof <- tanzil_translation("https://tanzil.net/trans/en.yusufali")
trans_id_imra <- tanzil_translation("https://tanzil.net/trans/id.indonesian")






#unnest token--------

trans_malay_basmeih_token <- trans_en_sahih$translation_text %>% 
  unnest_tokens(word, translation) %>% 
  filter(surah_no == 10)


#Word cloud ------------


trans_malay_basmeih_token_surah <- trans_malay_basmeih_token %>% 
  count(word) 
  

wordcloud(trans_malay_basmeih_token_surah$word, trans_malay_basmeih_token_surah$n, max.words = 200, min.freq = 1,
          random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))





#Word cloud function------------


wordcloud_trans <- function(tanzil_trans_object, surah_number = (1:114), max_word = 200, min_freq = 1){
  
  surah_number <- as.integer(surah_number)
  
  if(!"translationList" %in% class(tanzil_trans_object)){
    stop("Input argument is not tanzil translation object, please create this object with tanzil_translation() function")
  }
  
  if(!is.integer(surah_number) || surah_number > 114 || surah_number < 1) {
    stop("surah_number input argument must be a integer value between 1 to 114")
  }
  
  token_trans <- tanzil_trans_object[[1]] %>% 
    filter(surah_no %in% surah_number) %>% 
    unnest_tokens(word, translation) %>% 
    count(word)
    
  wordcloud(token_trans$word, token_trans$n, max.words = max_word, min.freq = min_freq,
            random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
    
  
}


#Term-frequency---------------

#calculate number of specific word and total number of words used in surah
wordcount_trans_en_sahih <- trans_en_sahih$translation_text %>% 
  unnest_tokens(word, translation) %>% 
  count(surah_no, word, sort = TRUE)

wordcount_trans_en_sahih2 <- trans_en_sahih$translation_text %>% 
  unnest_tokens(word, translation) %>% 
  group_by(surah_no, word) %>% 
  summarise(n())


totalword_trans_en_sahih <- wordcount_trans_en_sahih %>% 
  group_by(surah_no) %>% 
  summarize(total = sum(n))


join_wordcount_totalcount_en_sahih <- left_join(wordcount_trans_en_sahih, totalword_trans_en_sahih, by = "surah_no") %>% 
  mutate(tf = n/total)

ggplot(join_wordcount_totalcount_en_sahih %>% filter(surah_no == c(2,110,111,112,113,114)), aes(tf, fill = surah_no)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~surah_no, ncol = 2, scales = "free_y") +
  labs(title="tf Histogram", x = "term frequency")




#term frequency function-------

tf_trans <- function(tanzil_trans_object, surah_number = (1:114)){
  
  surah_number <- as.integer(surah_number)
  
  
  if(!"translationList" %in% class(tanzil_trans_object)){
    stop("Input argument is not tanzil translation object, please create this object with tanzil_translation() function")
  }
  
  
  if(!is.integer(surah_number) || surah_number > 114 || surah_number < 1) {
    stop("surah_number input argument must be a integer value between 1 to 114")
  }
  
  count_word_trans <- tanzil_trans_object[[1]] %>% 
    filter(surah_no %in% surah_number) %>% 
    unnest_tokens(word, translation) %>% 
    count(surah_no, surah_title_en, word, sort = TRUE)
  
  total_word_trans <- count_word_trans %>% 
    group_by(surah_no) %>% 
    summarize(total = sum(n))
  
  join_count_total_word_trans <- left_join(count_word_trans, total_word_trans, by = "surah_no") %>% 
    mutate(tf = n/total)
}

#eg:

tf_trans_shahih <- tf_trans(trans_en_sahih)
tf_trans_surah_short_shahih <- tf_trans(trans_en_sahih, c(113, 112, 109, 111, 114, 110))
tf_trans_surah_short_yusoff <- tf_trans(trans_en_yusof, c(113, 112, 109, 111, 114, 110))


#plot tf-histogram

tf_plot_malay_new <- ggplot(tf_trans_surah_short_malay, aes(tf, fill = surah_no)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~surah_no, ncol = 3, scales = "free_y") +
  labs(title=attributes(test_new_malay)[[3]], x = "term frequency")



tf_plot_yusoff <-  ggplot(tf_trans_surah_short_yusoff, aes(tf, fill = surah_no)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~surah_no, ncol = 3, scales = "free_y") +
  labs(title="tf Histogram Yusof", x = "term frequency")


tf_plot_malay <- ggplot(tf_trans_surah_short_malay, aes(tf, fill = surah_no)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~surah_no, ncol = 3, scales = "free_y") +
  labs(title="tf Histogram malay old", x = "term frequency")


tf_plot_china <- ggplot(tf_trans_surah_short_china, aes(tf, fill = surah_no)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~surah_no, ncol = 3, scales = "free_y") +
  labs(title="tf Histogram China", x = "term frequency")






grid.arrange(tf_plot_malay_new, tf_plot_malay, tf_plot_yusoff, tf_plot_china)


#add attribute to object

attr(trans_en_sahih, "trans_indicator") <- trans_en_sahih[[2]] %>% 
  select(value) %>% 
  slice(2,3) %>% 
  pull() %>% 
  paste(collapse = "-")


# Test ----------------------





count_word_trans <- trans_en_sahih[[1]] %>% 
  filter(surah_no %in% c(113, 112, 109, 111, 114, 110)) %>% 
  unnest_tokens(word, translation) %>% 
  count(surah_no, surah_title_en, word, sort = TRUE)

total_word_trans <- count_word_trans %>% 
  group_by(surah_no) %>% 
  summarize(total = sum(n))

join_count_total_word_trans <- left_join(count_word_trans, total_word_trans, by = "surah_no") %>% 
  mutate(tf = n/total)





join_count_total_word_trans %>% 
  filter(surah_no == 109) %>% 
  summarise(sum(tf))








#pull translation info

trans_info_shahih <- trans_en_sahih[[2]] %>% 
  select(value) %>% 
  slice(2,3) %>% 
  pull() %>% 
  paste(collapse = "-")


trans_info_yusoff <- trans_en_yusof[[2]] %>% 
  select(value) %>% 
  slice(2,3) %>% 
  pull() %>% 
  paste(collapse = "-")


trans_info_malay <- trans_malay_basmeih[[2]] %>% 
  select(value) %>% 
  slice(2,3) %>% 
  pull() %>% 
  paste(collapse = "-")


trans_info_china <- trans_china[[2]] %>%
  select(value) %>% 
  slice(2,3) %>% 
  pull() %>% 
  paste(collapse = "-")
                                  


%>% 
  filter(info %in% c("Translator", "Language"))


join_wordcount_totalcount_en_sahih %>% 
  filter(surah_no == 40) %>% 
  summarise(sumTf = sum(tf))


library(dplyr)
library(janeaustenr)


d <- tibble(txt = prideprejudice)
d

d %>%
  unnest_tokens(word, txt)


d %>%
  unnest_tokens(sentence, txt, token = "sentences")

d %>%
  unnest_tokens(ngram, txt, token = "ngrams", n = 2)

d %>%
  unnest_tokens(chapter, txt, token = "regex", pattern = "Chapter [\\\\d]")

d %>%
  unnest_tokens(shingle, txt, token = "character_shingles", n = 4)

# custom function
d %>%
  unnest_tokens(word, txt, token = stringr::str_split, pattern = " ")