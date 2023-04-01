#' Pull Quran Translation From tanzil.net
#'
#' @param urlLink url translation link from https://tanzil.net/trans/
#'
#' @return list of translation text and info
#' 
#' @importFrom readr read_delim
#' @importFrom stringr str_split_fixed str_detect str_remove str_trim
#' @importFrom tidyr separate
#' @import dplyr
#' 
#' 
#' @export
#'
#' @examples
#' trans_malay_basmeih_list <- tanzil_translation("https://tanzil.net/trans/ms.basmeih")
#' trans_en_sahih_list <- tanzil_translation("https://tanzil.net/trans/en.sahih")
#' 
tanzil_translation <-
function(urlLink){
  
  trans <- read_delim(urlLink, delim = "\n", col_names = FALSE)
  
  trans_text <- trans[[1]][1:6236] %>% 
    str_split_fixed("\\|", 3) %>% 
    as.data.frame() %>% 
    rename("surah_no" = names(.)[1], "ayah_no" = names(.)[2], "translation" = names(.)[3]) %>% 
    mutate(surah_no = as.integer(surah_no)) %>% 
    mutate(ayah_no = as.integer(ayah_no))
  
  trans_info <- trans[6237:6247, ] %>% 
    filter(str_detect(X1, "\\w+")) %>% 
    mutate(X1=str_remove(X1,"^#")) %>% 
    mutate(X1 = str_trim(X1)) %>% 
    separate(X1 ,c("info", "value"), sep = ":") %>% 
    slice(-1) %>% 
    mutate(value = trimws(value))
  
  trans_list <- list(trans_text, trans_info)
  names(trans_list) <- c("translation_text", "translation_info")
  class(trans_list) <- append("translationList", class(trans_list))
  
  
  attr(trans_list, "trans_indicator") <- trans_info %>% 
    select(value) %>% 
    slice(2,3) %>% 
    pull() %>% 
    paste(collapse = "-")
  
  
  invisible(trans_list)
  
}





