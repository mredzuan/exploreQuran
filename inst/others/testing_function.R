
trans_en_sahih <- tanzil_translation("https://tanzil.net/trans/en.sahih")
trans_en_yusof <- tanzil_translation("https://tanzil.net/trans/en.yusufali")


tf_trans_shahih <- tf_trans(trans_en_sahih)
tf_trans_surah_short_shahih <- tf_trans(trans_en_sahih, c(113, 112, 109, 111, 114, 110))
tf_trans_surah_short_yusoff <- tf_trans(trans_en_yusof, c(113, 112, 109, 111, 114, 110))


wordcloud_trans(trans_en_sahih, c(1,2,3))
