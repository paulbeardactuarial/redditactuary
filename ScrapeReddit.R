library(RedditExtractoR)
library(tidyverse)


# data scrape - ActuaryUK
top.actuary.posts <- RedditExtractoR::find_thread_urls(
  sort_by = "top",
  subreddit="ActuaryUK",
  period="all")

top.url <- top.actuary.posts %>% .$url
df <- get_thread_content(top.url)

saveRDS(df,"./Data/ActuaryUK.all.threads.rds")


# data scrape - Actuary

# data scrape
top.actuary.posts <- RedditExtractoR::find_thread_urls(
  sort_by = "top",
  subreddit="Actuary",
  period="all")

top.url <- top.actuary.posts %>% .$url
df <- get_thread_content(top.url)

write_rds(df,"./Data/Actuary.all.threads.rds")


