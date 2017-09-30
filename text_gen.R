rm(list = ls())
gc()

library(data.table)
library(magrittr)
library(tidyverse)
library(tidytext)
library(stringr)

tmp.file <- tempfile(fileext = '.zip')
download.file('http://www.ruscorpora.ru/ngrams/2grams-3.zip', destfile = tmp.file)
text <- read_lines(tmp.file)
rm(tmp.file)

text %<>% 
  str_replace_all('[:punct:]', '') %>% 
  str_replace_all('\\t+', '\t') %>% 
  str_replace_all('\\s+', ' ') %>% 
  str_replace_all('ё', 'е') %>% 
  str_to_lower()

tidy.text <- tibble(text = text)
tidy.text %<>% 
  separate(text, c('n', 'word1', 'word2'), sep = ' ', convert = T)

text_generater <- function(first_word, text_df, text_length = 10, remove_sw = TRUE) {
  if (remove_sw == TRUE) {
    sw <- read_csv('https://goo.gl/pfpUrB', col_names = FALSE)$X1
  }
  
  tidy.text <- text_df %>%
  {if(remove_sw) filter(., !word1 %in% sw) else .} %>% 
  {if(remove_sw) filter(., !word2 %in% sw) else .} %>% 
    filter(!grepl(pattern = '\\d+', word1)) %>%
    filter(!grepl(pattern = '\\d+', word2)) %>%
    unite(bigram, word1, word2, sep = ' ', remove = FALSE) %>% 
    mutate(rowno = row_number(), used = FALSE)
  
  sub_funct <- function(word) {
    res.df <- tidy.text %>% 
      filter(word1 == word, used == FALSE) %>% 
      select(bigram, word2, rowno) %>% 
      head(1)
    if (nrow(res.df) == 0) {
      res.df <- tidy.text %>% 
        filter(word1 == word) %>% 
        select(bigram, word2, rowno) %>% 
        head(1)
    }
    setDT(tidy.text)
    tidy.text[rowno == res.df$rowno, used := TRUE]
    
    return(res.df %>% select(bigram, word2))
  }
  
  res <- c()
  next.word <- first_word
  
  for (i in 1:(text_length - 1)) {
    sf <- sub_funct(next.word)
    if (i == 1) {
      res <- c(res, sf$bigram)
    } else {
      res <- c(res, sf$word2)
    }
    next.word <- sf$word2
  }
  
  return(paste(res, collapse = ' '))
  #return(tidy.text)
}

text_generater('тест', tidy.text, remove_sw = T, text_length = 10)



