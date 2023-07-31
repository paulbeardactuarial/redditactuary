# function that takes comments data.frame and filters out comments with "exam" arguement in
# then measures the sentiment of all other words in these remaining comments
exam.sentiment <- function(exam, comments) {

  is.exam.comment <- str_detect(string=comments$comments, pattern=exam)
  exam.comments <- filter(comments,is.exam.comment)

  words <- exam.comments %>% unnest_tokens(.,token = "words",output="word",input=comments)

  # All words
  word.freq <- anti_join(words,stop_words) %>%
    filter(str_detect(word, pattern = "[a-z]")) %>%
    group_by(word) %>%
    summarise(count=n(),score=sum(score,na.rm=T)) %>%
    filter(!str_detect(word,pattern = "^c[bmsp]\\d$") & !str_detect(word,pattern = "^s[ap]\\d$")) %>%
    left_join(get_sentiments())

}
