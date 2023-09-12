library(RedditExtractoR)
library(tidyverse)

# select your subreddit to scrape...
subreddit <- "ActuaryUK" #alternatively scrape... "Actuary"

# get the top 1000 thread URLs for that subreddit and place in top.url vector
top.actuary.posts <- RedditExtractoR::find_thread_urls(
  sort_by = "top",
  subreddit = subreddit,
  period="all")
top.url <- top.actuary.posts %>% .$url

# empty list to place results in
thread.so.far <- list()

# loop to pull through the top.url info with get_thread_content
# reddit was only returning some threads because of too much requests (error 429)
# loop captures the successful retrievals, and retains the unsuccessful URLs to try again
# not great code as will go on forever if top.url won't whittle down... if not resolved after an hour or two likely needs aborting and investigating
while (length(top.url) > 1)   {

  # use map/safely to extract thread content for each url in vector...
  # note that get_thread_content would also work on a vector directly without need for map...
  # ...however doing this way so can extract successful results and filter them out for re-attempts
  df <- map(top.url, safely(get_thread_content))

  #need to close connections as leaving open creates error
  closeAllConnections()

  # extract succesful results and add the threads.so.far
  nu.df <- transpose(df)
  successful.pull <- !map_lgl(nu.df$result, is.null)
  thread.so.far <- c(thread.so.far, nu.df$result[successful.pull])

  # remove sucessful results from top.url vector - ready to reattempt
  top.url <- top.url[!successful.pull]

  # wait 5 mins... don't know how long should wait but putting cooldown period in to reduce likelihood of error 429
  Sys.sleep(300)
}

# print out our results
write_rds(thread.so.far,paste("./Data/",subreddit,".all.threads.rds",sep=""))




thread.so.far <- read_rds(paste("./Data/",subreddit,".all.threads.rds",sep=""))

clean.threads <- function(thread.so.far) {
cleaned.threads <- map(thread.so.far, ~.$comments)
cleaned.threads <- cleaned.threads[map_lgl(cleaned.threads, function(x) length(x)!=0)]
cleaned.threads <- map(cleaned.threads, function(x) select(x, -comment_id))
cleaned.threads <- cleaned.threads %>% bind_rows()
return(cleaned.threads)
}

cleaned.threads <- clean.threads(thread.so.far)
# print out our results
write_rds(cleaned.threads,paste("./Data/",subreddit,".all.comments.rds",sep=""))

