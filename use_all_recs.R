# Snippet to consume all recommendations 


# update user interests
for(i in 1:dim(recs)[1]){
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
  
}


# update exposure counts in each step for all recommendations
for(i in 1:dim(recs)[1]){
  position <- unlist(recs[i,1] )
  exposure[position, steps] + 1 -> temp
  exposure[position, steps] <- temp
}