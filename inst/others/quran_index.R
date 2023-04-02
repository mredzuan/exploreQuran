

# Example translation--------

trans_en_sahih <- tanzil_translation("https://tanzil.net/trans/en.sahih")

trans_malay_basmeih <- tanzil_translation("https://tanzil.net/trans/ms.basmeih")


# Quran Index -----------


quran_index <- quRan::quran_en_sahih %>% 
 distinct(surah_id, surah_title_ar, surah_title_en, surah_title_en_trans, revelation_type)

quran_arab <- quRan::quran_ar



#Test -----------------

trans_china <- tanzil_translation("https://tanzil.net/trans/zh.jian")


malay_trans <- trans_malay_basmeih$translation_text %>% 
  left_join(quran_index, by = c("surah_no"="surah_id", "ayah_no"="ayah"))