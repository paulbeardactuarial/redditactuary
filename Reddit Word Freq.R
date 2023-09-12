library(RedditExtractoR)
library(tidyverse)
library(tidytext)
library(SnowballC)
library(wordcloud2)
source("./Functions/cleanse.comments.R")

#data processing
df <- readRDS("./Data/ActuaryUK.all.comments.rds")
df$comment <- str_to_lower(df$comment)
comments <- data.frame(comments = df$comment, date=df$date)

# cleanse comments using word stemming to rename similar words to a single representative word
word.df <- cleanse.comments(comments)

# find frequency of each stem in each month
# filter out numbers, as well as exam names (these will be analysed in a separate part)
# convert final table from stem to word by joining on our word.stem.map we created in previous section
word.freq <- word.df %>%
  filter(str_detect(word, pattern = "[a-z]")) %>%
  group_by(word) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  filter(!str_detect(word,pattern = "^c[bmsp]\\d$") & !str_detect(word,pattern = "^s[ap]\\d$")) %>%
  rename(freq=count)

# apply our word.freq table to a wordcloud
color_palette <- c("#FF8b60", "#9494FF") # these are the upvote/downvote colours of reddit
wc <- wordcloud2(word.freq[1:50,],
           color=rep_len(c(color_palette), nrow(word.freq)),
           size = .75)

wc
# wordcloud was saved using "Export" feature of RStudio



