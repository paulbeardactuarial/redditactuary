
library(RedditExtractoR)
library(tidyverse)
library(tidytext)
source("./Functions/exam.sentiment.R")

#data processing
df <- readRDS("./Data/ActuaryUK.all.threads.rds")
df$comments$comment <- str_to_lower(df$comments$comment)
comments <- data.frame(comments = df$comments$comment, date=df$comments$date, score=df$comments$score)

# filter out just comments that mention an exam
exams<-c("CM1","CM2","CS1","CS2","CB1","CB2","CB3","CP1","CP2","CP3","SA\\\\d","SP\\\\d")
exams <- str_to_lower(exams)



es <- map(exams,.f=exam.sentiment,comments=comments)
names(es) <- exams

exam.sentiments <-bind_rows(es,.id="exam")

banned.words <- c("excel")

ws <- exam.sentiments %>%
  filter(!is.na(sentiment)) %>%
  filter(!word %in% banned.words) %>%
  group_by(exam,word,sentiment) %>%
  summarise(count=sum(count),score=sum(score,na.rm=T)) %>%
  arrange(desc(score))

word.levels <- ws %>% group_by(word) %>% summarise(score=sum(score)) %>% arrange(desc(score)) %>% .$word
exam.levels <- ws %>% group_by(exam) %>% summarise(score=sum(score)) %>% arrange(desc(score)) %>% .$exam

ws$word <- factor(ws$word, levels = word.levels)
ws$exam <- factor(ws$exam, levels = exam.levels)

ws.short <- filter(ws,as.integer(word) %in% c(1:20))

sensum <- ws %>% group_by(exam,sentiment ) %>% summarise(score=sum(score))

p <- sensum %>% filter(sentiment=="positive") %>% select(-sentiment) %>% rename("positive"="score")
n <- sensum %>% filter(sentiment=="negative") %>% select(-sentiment) %>% rename("negative"="score")

pn <- left_join(p,n) %>% mutate(ratio=positive/(positive+negative))  %>% arrange(ratio)
pn$exam <- factor(pn$exam, levels = pn$exam)

# overall sentiment plot
ggplot(pn, aes(x=exam,y=ratio-0.5)) +
  geom_col(fill="#FF5700") + #make it the same orange-red colour as reddit!
  scale_y_continuous(name="Proportion of Sentimental Words in Comments are Positive",
                     breaks=seq(-0.2,0.2,by=0.05),
                     labels=scales::percent(seq(-0.2,0.2,by=0.05)+0.5),
                     limits=c(-0.2,0.2)) +
  scale_x_discrete(name="Exam",labels=str_to_upper(pn$exam)) +
  theme_classic() +
  theme(legend.position = "none")



top.comments <- comments %>% arrange(desc(score)) %>% head(5) %>% .$comment


