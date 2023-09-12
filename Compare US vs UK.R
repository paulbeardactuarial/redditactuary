library(tidyverse)
library(patchwork)

source("./Functions/cleanse.comments.R")

#data processing - UK
act.uk <- readRDS("./Data/ActuaryUK.all.comments.rds")
act.uk$comment <- str_to_lower(act.uk$comment)
comments.uk <- data.frame(comments = act.uk$comment, date=act.uk$date, score=act.uk$score)

#data processing - US
act.us <- readRDS("./Data/Actuary.all.comments.rds")
act.us$comment <- str_to_lower(act.us$comment)
comments.us <- data.frame(comments = act.us$comment, date=act.us$date, score=act.us$score)

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

all.words <- rbind(
data.frame(word.freq.uk, subreddit = "ActuaryUK"),
data.frame(word.freq.us, subreddit = "Actuary")
)


# list of exams - these are removed from analysis as they would top the TF-IDF tables and this is not an interesting result
us.exams <- c("P","FM","IFM","LTAM","MAS-I","MAS-II","MFE","CBT","SRM","STAM") %>% str_to_lower()
uk.exams<-c("CM1","CM2","CS1","CS2","CB1","CB2","CB3","CP1","CP2","CP3","SA\\d","SP\\d") %>% str_to_lower()

is.exam.string <- c(map(uk.exams, function(x) str_detect(all.words$stem, x)),
                    map(us.exams, function(x) str_detect(all.words$stem, x))) %>% reduce(`|`)
all.words.minus.exams <- filter(all.words, !is.exam.string) %>% filter(!str_detect(string=word,pattern="\\d"))

# also removing the following. These are uninteresting results for TF-IDF... either USA spelling variants not use by the UK or websites.
manual.removals <- c("favorite","color","mom","labor","soa.org","actuaries.org.uk","www.soa.org")

tf.idf.reddit <- all.words.minus.exams %>%
  bind_tf_idf(stem, subreddit, freq) %>%
  arrange(desc(tf_idf)) %>%
  filter(!word %in% manual.removals)

tf_idf_plot <- function(data,
                        subreddit.filter,
                        n.bars,
                        fill.colour) {
  ggplot(data %>% filter(subreddit==subreddit.filter) %>% head(n.bars),
                  aes(x=fct_reorder(word,tf_idf,.desc=T),
                      y=tf_idf)) +
  geom_col(fill=fill.colour, show.legend = TRUE) +
  labs(y="TF-IDF", x="Word", title=paste("r/",subreddit.filter, sep="")) +
  scale_fill_discrete(name=subreddit.filter) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_blank())
}

us.plot <- tf_idf_plot(data = tf.idf.reddit, subreddit.filter = "Actuary", n.bars = 15 , fill.colour = "dodgerblue2")
uk.plot <- tf_idf_plot(data = tf.idf.reddit, subreddit.filter = "ActuaryUK", n.bars = 15 , fill.colour = "firebrick1")


pp.ukus <- uk.plot / us.plot


ggsave("pp.ukus.jpg")






# sentiments
if (FALSE) {
uk.sent <- word.act.uk %>%
  left_join(get_sentiments()) %>%
  group_by(sentiment) %>%
  summarise(score=sum(score))

us.sent <- word.act.us %>%
  left_join(get_sentiments(), relationship = "many-to-many") %>%
  group_by(sentiment) %>%
  summarise(score=sum(score))
us.sent$score / sum(us.sent$score)

uk.sent$score / sum(uk.sent$score)
}




#top comment
top.comment <- act.uk %>% arrange(desc(score)) %>% head(1) %>% .$comment
act.uk %>% arrange(desc(score)) %>% head(1) %>% .$comment
