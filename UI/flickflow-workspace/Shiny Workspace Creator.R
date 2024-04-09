# IMPORTS ---------------------------------------------------------------

library(dplyr)
library(mongolite)
library(jsonlite)

movies = read.csv('/Users/kelvinervais/Desktop/flickflow/UI/flickflow-workspace/movie_data.csv')
movies = movies %>% distinct(id, .keep_all = TRUE)




# MONGO FUNCTIONS AND IMPORTS ---------------------------------------------------------------

collection_name = "ratingsCol"

mongo_database = mongo(db = "ratingsDB", 
                       collection = collection_name,  
                       url="mongodb+srv://jgoldsher:Leon422@ratingscluster.wqwgnad.mongodb.net/")

toJSON_updater = function(dataframe) {
  result = toJSON(dataframe) %>% 
    gsub('^\\[|\\]$', '', .)
  
  result
}

upsert_data = function(upsert_content, user) {
  
  
  mongo_database$update(paste0('{"entry_type": "user_response",
                           "User": "',user,'"}'),
                        paste0('{"$set":', toJSON_updater(upsert_content), '}'),
                        upsert = TRUE)
  
}
#mongo_database$drop()
data <- mongo_database$find('{}')



# USER SIM FOR COLLAB INIT ---------------------------------------------------------------
cosine_similarity <- function(x, y) {
  dot_product <- sum(x * y)
  magnitude_x <- sqrt(sum(x^2))
  magnitude_y <- sqrt(sum(y^2))
  similarity <- dot_product / (magnitude_x * magnitude_y)
  return(similarity)
}
# Function to simulate user preferences with adjusted weights for COMP_ columns
simulate_user_preferences <- function() {
  user_prefs <- numeric(length(user_pref_columns))
  for (i in seq_along(user_pref_columns)) {
    if (grepl("COMP_", user_pref_columns[i])) {
      # Calculate weight based on proportion of occurrences
      weight <- sum(movies[[user_pref_columns[i]]] == 1) / nrow(movies)
      value <- sample(0:5, 1, prob = c(1 - weight, rep(weight, 5)))  # Cap the value at 5
    } else if (grepl("percentile", user_pref_columns[i])) {
      value <- runif(1, min = 0.5, max = 1)  # Generate a random value between 0.5 and 1 for percentile columns
    } else {
      value <- sample(0:3, 1)  # Simulate counts between 0 and 3 for non-percentile and non-COMP_ columns
    }
    user_prefs[i] <- value
  }
  return(user_prefs)
}
user_pref_columns <- c(grep("^(GENRE_|DECADE_|COMP_|.*_percentile)", names(movies), value = TRUE))
user_pref_columns <- gsub("'", "_", user_pref_columns)
user_pref_columns <- gsub(" ", "_", user_pref_columns)
user_pref_columns <- gsub("\\.", "_", user_pref_columns)
real_users = data %>% filter(entry_type == 'user_profile') %>% select(username, all_of(user_pref_columns))
real_users$popularity_percentile <-  ecdf(movies$popularity)(real_users$popularity_percentile)
real_users$vote_average_percentile <-  ecdf(movies$vote_average_percentile)(real_users$vote_average_percentile)
real_users$revenue_percentile <-  ecdf(movies$revenue_percentile)(real_users$revenue_percentile)
real_users$budget_percentile <-  ecdf(movies$budget_percentile)(real_users$budget_percentile)
real_users$runtime_percentile <-  ecdf(movies$runtime_percentile)(real_users$runtime_percentile)

num_users <- 100
user_data <- replicate(num_users, simulate_user_preferences())

# Convert simulated user preferences to dataframe
user_df <- as.data.frame(t(user_data))
colnames(user_df) <- user_pref_columns

# Add user ID column
sim_ids <- replicate(num_users, paste0(sample(letters, 6, replace = TRUE), collapse = ""))

user_df$username <- sim_ids
user_df = user_df %>% select(username, all_of(user_pref_columns))


user_df = rbind(user_df,real_users)
num_users = nrow(user_df)
# Compute similarity matrix between users
similarity_matrix <- matrix(NA, nrow = num_users, ncol = num_users)
for (i in 1:num_users) {
  for (j in 1:num_users) {
    similarity_matrix[i, j] <- cosine_similarity(user_df[i, user_pref_columns], user_df[j, user_pref_columns])
  }
}



# Scale similarity scores to a range of 0 to 1
similarity_matrix <- (similarity_matrix - min(similarity_matrix, na.rm = TRUE)) / (max(similarity_matrix, na.rm = TRUE) - min(similarity_matrix, na.rm = TRUE))
rownames(similarity_matrix) = user_df$username



objects_list <- list(movies = movies, similarity_matrix = similarity_matrix, user_df = user_df)

# Save the list to an RDS file
saveRDS(objects_list, file = "~/Desktop/flickflow/UI/flickflow/flickflow_base_workspace.rds")
