library(RedditExtractoR)
library(tidyverse)
library(tidytext)
library(SnowballC)
library(wordcloud2)

#data processing
df <- readRDS("ActuaryUK.all.threads.rds")
df$comments$comment <- str_to_lower(df$comments$comment)
comments <- data.frame(comments = df$comments$comment, date=df$comments$date)

# getting a dataframe of unnested words and the date(s) they appear
word.df <- comments %>% unnest_tokens(.,token = "words",output="word",input=comments)
word.df$word <- str_remove_all(word.df$word,"'")
word.df$month <- format(as.Date(word.df$date), "%Y-%m")
word.df$year <- format(as.Date(word.df$date), "%Y")

# remove stop words
word.df <- anti_join(word.df,stop_words)

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
  filter(count==max(count)) %>%
  ungroup() %>%
  select(word, stem)


# find frequency of each stem in each month
# filter out numbers, as well as exam names (these will be analysed in a separate part)
# convert final table from stem to word by joining on our word.stem.map we created in previous section
word.freq.month <- word.df %>%
  select(-word) %>%
  filter(str_detect(stem, pattern = "[a-z]")) %>%
  group_by(stem, month) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  filter(!str_detect(stem,pattern = "^c[bmsp]\\d$") & !str_detect(stem,pattern = "^s[ap]\\d$")) %>%
  left_join(word.stem.map) %>%
  select(word, month, freq=count)

# use term frequency inverse document frequency (bind_tf_idf) scores to determine which words appeared in unusually high
  # amounts for each month
# filter out the highest score per month to get "word of the month"
  wfm <-
    filter(word.freq.month,
           !str_detect(word, pattern="\\d")
           ) %>%
    bind_tf_idf(word, month, freq) %>%
    group_by(month) %>%
    filter(tf_idf==max(tf_idf)) %>%
    arrange(desc(month)) %>%
    filter(month >= "2022-06",
           month <= "2023-05")

  write.csv(wfm,"wfm.csv")


    # find frequency of each stem
  # filter out numbers, as well as exam names (these will be analysed in a separate part)
  # convert final table from stem to word by joining on our word.stem.map we created in previous section
  word.freq <- word.df %>%
    select(-word) %>%
    filter(str_detect(stem, pattern = "[a-z]")) %>%
    group_by(stem) %>%
    summarise(count=n()) %>%
    arrange(desc(count)) %>%
  filter(!str_detect(stem,pattern = "^c[bmsp]\\d$") & !str_detect(stem,pattern = "^s[ap]\\d$")) %>%
    left_join(word.stem.map) %>%
    select(word, freq=count)


# apply our word.freq table to a wordcloud
color_palette <- c("grey", "blue")
wordcloud2(word.freq[1:50,],
           #color = sentiment,
           color=rep_len(c(color_palette), nrow(word.freq)),
           size = .75)


#top comment
top.comment <- df$comments %>% arrange(desc(score)) %>% head(5) %>% .$comment
