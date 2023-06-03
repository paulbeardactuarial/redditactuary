library(RedditExtractoR)
library(tidyverse)


# data scrape
top.actuary.posts <- RedditExtractoR::find_thread_urls(
  sort_by = "top",
  subreddit="ActuaryUK",
  period="all")

top.url <- top.actuary.posts %>% .$url
df <- get_thread_content(top.url)

saveRDS(df,"ActuaryUK.all.threads.rds")
