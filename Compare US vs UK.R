

#data processing - UK
act.uk <- readRDS("./Data/ActuaryUK.all.threads.rds")
act.uk$comments$comment <- str_to_lower(act.uk$comments$comment)
comments.uk <- data.frame(comments = act.uk$comments$comment, date=act.uk$comments$date)

#data processing - US
act.us <- readRDS("./Data/Actuary.all.threads.rds")
act.us$comments$comment <- str_to_lower(act.us$comments$comment)
comments.us <- data.frame(comments = act.us$comments$comment, date=act.us$comments$date)

# cleanse comments using word stemming to rename similar words to a single representative word
word.act.uk <- cleanse.comments(comments.uk)
word.act.us <- cleanse.comments(comments.us)

# find frequency of each stem in each month
# filter out numbers, as well as exam names (these will be analysed in a separate part)
# convert final table from stem to word by joining on our word.stem.map we created in previous section
group.words <- function(word.act) {
word.freq <- word.act %>%
  filter(str_detect(word, pattern = "[a-z]")) %>%
  group_by(word) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  rename(freq=count) %>%
  mutate(rank=rank(desc(freq)))
word.freq$stem <- SnowballC::wordStem(word.freq$word)
return(word.freq)
}

word.freq.uk <- group.words(word.act.uk)
word.freq.us <- group.words(word.act.us)

total.uk <- word.freq.uk$freq %>% sum()
total.us <- word.freq.us$freq %>% sum()
words.per.n <- 10000

word.freq.uk <- word.freq.uk %>% mutate(wpn = freq * words.per.n / total.uk)
word.freq.us <- word.freq.us %>% mutate(wpn = freq * words.per.n / total.us)

comparing.act <- left_join(word.freq.uk, word.freq.us, by = "stem") %>%
  mutate(diff.wpn = wpn.x - wpn.y) %>%
  mutate(diff.rank = rank.x - rank.y) %>%
  filter(pmin(rank.x,rank.y)<=50) %>%
  arrange(diff.rank)
View(comparing.act)

comparing.act$word.x <- factor(comparing.act$word.x, levels = comparing.act$word.x)

ggplot(comparing.act, aes(x=word.x,y=diff.rank)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))

#
# us.vs.uk <-
# rbind(
#   data.frame(stem = word.freq.uk$stem, subreddit = "ActuaryUK", freq = word.freq.uk$freq),
#   data.frame(stem = word.freq.us$stem, subreddit = "Actuary", freq = word.freq.us$freq)
# )
#
# differences <- us.vs.uk %>%
#   bind_tf_idf(stem, subreddit, freq) %>%
#   arrange(desc(tf_idf)) %>%
#   filter(!str_detect(stem, pattern = ".\\d"),
#          freq>100)
# View(differences)








#top comment
top.comment <- act.us$comments %>% arrange(desc(score)) %>% head(5) %>% .$comment

