library(RedditExtractoR)
library(tidyverse)
library(tidytext)
library(SnowballC)
library(wordcloud2)
source("./Functions/cleanse.comments.R")

#data processing
df <- readRDS("./Data/ActuaryUK.all.threads.rds")
df$comments$comment <- str_to_lower(df$comments$comment)
comments <- data.frame(comments = df$comments$comment, date=df$comments$date)

# cleanse comments using word stemming to rename similar words to a single representative word
word.df <- cleanse.comments(comments)

# find frequency of each stem in each month
# filter out numbers, as well as exam names (these will be analysed in a separate part)
# convert final table from stem to word by joining on our word.stem.map we created in previous section
word.freq.month <- word.df %>%
  filter(str_detect(word, pattern = "[a-z]")) %>%
  group_by(word, month) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  filter(!str_detect(word,pattern = "^c[bmsp]\\d$") & !str_detect(word,pattern = "^s[ap]\\d$")) %>%
  rename(freq=count)

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
color_palette <- c("#FF8b60", "#9494FF") # these are the upvote/downvote colours of reddit
wc <- wordcloud2(word.freq[1:50,],
           color=rep_len(c(color_palette), nrow(word.freq)),
           size = .75)


# SAVE wordcloud...

# install webshot
library(webshot)
webshot::install_phantomjs()

# save it in html
library("htmlwidgets")
saveWidget(wc,"wc.html",selfcontained = F)

# and in png or pdf
webshot("wc.html","wc.png", delay =5, vwidth = 480, vheight=480)



#top comment
top.comment <- df$comments %>% arrange(desc(score)) %>% head(5) %>% .$comment
