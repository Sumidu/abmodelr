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
interest_ressource <- rep(config$n_topics * 3, config$n_users)
topic_interests <- data.frame(user_ids)
for (i in 1:config$n_topics) {
  topic_interest_level <- data.frame(runif(config$n_users))
  names(topic_interest_level) <- paste0("topic_", i)
  topic_interests <- topic_interests %>% bind_cols(topic_interest_level)
}

user <- data.frame(topic_interests, interest_ressource)


# posts generation  ----

news_ids <- 1:config$n_newsposts

topic_relevances <- data.frame(news_ids)
for (i in 1:config$n_topics) {
  topic_relevance <- data.frame(runif(config$n_newsposts))
  names(topic_relevance) <- paste0("topic_", i)
  topic_relevances <- topic_relevances %>% bind_cols(topic_relevance)
}

news_posts <- data.frame(topic_relevances)
news_posts %>% select(starts_with("topic")) %>% mutate(sumtopics = rowSums(.)) %>% select(sumtopics) -> sumcol
news_posts %>% bind_cols(sumcol) -> news_posts

news_scores <- runif(config$n_newsposts, min = 1, max = config$topic_limit)

news_posts %>% select(starts_with("topic")) -> matrix_of_initial_values
updated_topics <- (matrix_of_initial_values / t(sumcol)) 

news_posts <- bind_cols(data.frame(news_ids), updated_topics, data.frame(news_scores))



# initilize recommender


user %>% select(starts_with("topic")) -> mat_a
news_posts %>% select(starts_with("topic")) -> mat_b
cosine_matrix <- matrix(c(0), nrow = config$n_users, ncol = config$n_newsposts)
for(i in 1:config$n_users) {
  for(j in 1:config$n_newsposts) {
    cosine_matrix[i,j] <- lsa::cosine(unlist(mat_a[i,]), unlist(mat_b[j,]))
  }
}

cosine_matrix

generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  res <- top_n(df, n) 
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}




exposure <- matrix(c(0), nrow = config$n_newsposts, ncol = config$n_steps)


# run all simulation steps
for (steps in 1:config$n_steps) {
  # for all users
  for(user_id in 1:config$n_users){
    # generate recommendations 
    recs <- generate_topn_rec(user_id, cosine_matrix = cosine_matrix, 10)
    
    
    
    # update user interests
    i <- sample(1:dim(recs)[1], 1)
      position <- unlist(recs[i,1] )
      news_posts[position,] %>% select(starts_with("topic")) -> post_scores
      user[user_id, ] %>% select(starts_with("topic")) -> user_interest
      user_interest <- user_interest + post_scores
      
      #noramlize if interest_ressource is maxed out
      if(sum(user_interest) > user[user_id,]$interest_ressource){
        user_interest <- user_interest / sum(user_interest) * user[user_id,]$interest_ressource
      }
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
}



View(exposure)


# save results 
results_data <- list(user = user, news_posts = news_posts, exposure = exposure)
rds_filename <- config$outputfilename
write_rds(results_data, rds_filename)




