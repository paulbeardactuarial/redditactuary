library(tidyverse)
library(tidytext)
library(SnowballC)

cleanse.comments <- function(comments) {

  # getting a dataframe of unnested words and the date(s) they appear
  word.df <- comments %>% unnest_tokens(.,token = "words",output="word",input=comments)
  word.df$word <- str_remove_all(word.df$word,"'")
  word.df$month <- format(as.Date(word.df$date), "%Y-%m")
  word.df$year <- format(as.Date(word.df$date), "%Y")

  # remove stop words
  word.df <- anti_join(word.df,stop_words)
  word.df <- anti_join(word.df,data.frame(word = stop_words$word %>% str_remove_all("'")))

  # apply wordStem() function to words$word
  # this will give the stem of a word rather than actual word... allows variants to be joined such as plurals
  word.df$stem <- SnowballC::wordStem(word.df$word)

  # create a word.stem.map
  # this picks the most common word found in the data for each stem type in the data
  # allows us to convert stem back to a single word as this is more intuitive to understand than a stem
  word.stem.map <- word.df %>%
    group_by(word, stem) %>%
    summarise(count=n()) %>%
    group_by(stem) %>%
    slice_max(n=1, order_by = count, with_ties = F) %>%
    ungroup() %>%
    select(word, stem)

  # join the word.stem.map to word.df and replace "word" values in word.df with the "word" values in word.stem.map
  # then regroup as some lines will be collaped
  output <- word.df %>% select(-word) %>% left_join(word.stem.map) %>% select(-stem)

}
