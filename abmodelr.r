library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)

#file <- selectFile(caption = "Select File", label = "Select",
#           path = getActiveProject(), filter = "All YAML Files (*.yml)",
#           existing = TRUE)

#config <- read_yaml(file)
config <- read_yaml("config.yml")

# User generation ----
user_ids <- 1:config$n_users
# initialize interest_resource with 3 time the amount of topics. So users have haed_room to increase their interest
interest_ressource <- rep(config$n_topics * 3, config$n_users)
# setup a data.frame for all users
topic_interests <- data.frame(user_ids)

# Generate interests for the users by topic
for (i in 1:config$n_topics) {
  # randomly assign interest to topics using the uniform distribution
  topic_interest_level <- data.frame(runif(config$n_users))
  # assign sensible column names
  names(topic_interest_level) <- paste0("topic_", i)
  # bind all topic interest to the user
  topic_interests <- topic_interests %>% bind_cols(topic_interest_level)
}

# create the actual user data frame
user <- data.frame(topic_interests, interest_ressource)


# posts generation  ----

# create ids
news_ids <- 1:config$n_newsposts
topic_relevances <- data.frame(news_ids)

# create news posts with certain topics
for (i in 1:config$n_topics) {
  # assign topic relevante by uniform distribution
  topic_relevance <- data.frame(runif(config$n_newsposts))
  names(topic_relevance) <- paste0("topic_", i)
  topic_relevances <- topic_relevances %>% bind_cols(topic_relevance)
}

news_posts <- data.frame(topic_relevances)
# calculate the sum of topic interests, to measure the "likeability"(?) of post
news_posts %>% select(starts_with("topic")) %>% mutate(sumtopics = rowSums(.)) %>% select(sumtopics) -> sumcol
news_posts %>% bind_cols(sumcol) -> news_posts


# Define a random likebility score for all news posts (uniform distribution, between 0 and topic-limit)
news_scores <- runif(config$n_newsposts, min = 1, max = config$topic_limit)
# get all topic values into a matrix for normalization
news_posts %>% select(starts_with("topic")) -> matrix_of_initial_values
# normalize all topic-value by the rowwise sum (generated above) - now sums should be 1
updated_topics <- (matrix_of_initial_values / t(sumcol)) 

# update the news_posts
news_posts <- bind_cols(data.frame(news_ids), updated_topics, data.frame(news_scores))



# initilize recommender
user %>% select(starts_with("topic")) -> mat_user
news_posts %>% select(starts_with("topic")) -> mat_posts
cosine_matrix <- matrix(c(0), nrow = config$n_users, ncol = config$n_newsposts)
for(i in 1:config$n_users) {
  for(j in 1:config$n_newsposts) {
    cosine_matrix[i,j] <- lsa::cosine(unlist(mat_user[i,]), unlist(mat_posts[j,]))
  }
}

#scosine_matrix



#' Generate recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}



# generate an empty exposre matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = config$n_newsposts, ncol = config$n_steps)


# run all simulation steps
pb <- txtProgressBar(min = 0, max = config$n_steps, initial = 0, char = "=",
               width = NA, title="Simulation Run", label, style = 3, file = "")
for (steps in 1:config$n_steps) {
  # for all users
  for(user_id in 1:config$n_users){
    # generate top 10 recommendations 
    recs <- generate_topn_rec(user_id, cosine_matrix = cosine_matrix, 10)
    
    
    
    # update user interests ----
    # draw one random sample from recommendations to consume
    i <- sample(1:dim(recs)[1], 1)
      # get position of newspost in news post data frame
      position <- unlist(recs[i,1] )
      
      # get scores from posts
      news_posts[position,] %>% select(starts_with("topic")) -> post_scores
      # get user interests
      user[user_id, ] %>% select(starts_with("topic")) -> user_interest
      # update user_interest from posts
      user_interest <- user_interest + post_scores
      
      #noramlize if interest_ressource is maxed out
      if(sum(user_interest) > user[user_id,]$interest_ressource){
        user_interest <- user_interest / sum(user_interest) * user[user_id,]$interest_ressource
      }
      
      # Update only top topics?
      
      # update user interest
      user[user_id,]  <- c(user_ids = user_id, user_interest, user[user_id,]$interest_ressource)     
      
    
    
    # update exposure counts in each step for all recommendations
      position <- unlist(recs[i,1] )
      exposure[position, steps] + 1 -> temp
      exposure[position, steps] <- temp
    
  }
  if(steps > 1){
    exposure[,steps] <- exposure[, steps] + exposure[, steps -1]
  }
  setTxtProgressBar(pb, steps)
}
close(pb)


#View(exposure)


# save results 
results_data <- list(user = user, news_posts = news_posts, exposure = exposure)
rds_filename <- config$outputfilename
write_rds(results_data, rds_filename)




