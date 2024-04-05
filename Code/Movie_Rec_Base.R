# Load required packages
library(dplyr)

# Step 1: Load Data
movies <- read.csv("~/Desktop/flickflow/Code/movie_data.csv")  

# Step 2: Prepare Data
# Extract relevant columns for user preferences (GENRE_, COMP_, DECADE_, and percentile columns)
user_pref_columns <- c(grep("^(GENRE_|DECADE_|COMP_|.*_percentile)", names(movies), value = TRUE))

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

# Function to compute cosine similarity between two vectors
cosine_similarity <- function(x, y) {
  dot_product <- sum(x * y)
  magnitude_x <- sqrt(sum(x^2))
  magnitude_y <- sqrt(sum(y^2))
  similarity <- dot_product / (magnitude_x * magnitude_y)
  return(similarity)
}

# Function to make recommendations for a user based on cosine similarity
make_recommendations <- function(user_id, k = 5, percentile_tolerance = 0.2) {
  # Find top k similar users to the given user (excluding the user itself)
  similar_users <- order(similarity_matrix[user_id, ], decreasing = TRUE)[-1][1:k]
  
  # Check if there are similar users
  if (length(similar_users) == 0) {
    print("No similar users found. Unable to make recommendations.")
    return(NULL)
  }
  
  # Aggregate preferences of similar users
  aggregated_preferences <- colMeans(user_df[similar_users, user_pref_columns], na.rm = TRUE)
  
  # Filter aggregated preferences to include only columns where mean value is greater than 0
  aggregated_preferences <- aggregated_preferences[aggregated_preferences > 0]  
  focus_columns = names(aggregated_preferences)
  # Filter movies to include only those that fit user preferences
  suitable_movies <- movies
  
  top_genre <- names(sort(aggregated_preferences, decreasing = TRUE))[1:3]
  suitable_movies <- suitable_movies %>% filter(
    !!sym(top_genre[1]) == T |
      !!sym(top_genre[2]) == T |
      !!sym(top_genre[3]) == T
  )
  
  top_company <- names(sort(aggregated_preferences[grep("COMP_", names(aggregated_preferences))], decreasing = TRUE))[1:3]
  suitable_movies <- suitable_movies %>% filter(
    !!sym(top_company[1]) == T |
      !!sym(top_company[2]) == T |
      !!sym(top_company[3]) == T
    
  )

  for (pref_column in focus_columns) {
    if (grepl("percentile", pref_column)) {
      lower_bound <- aggregated_preferences[pref_column] - percentile_tolerance
      upper_bound <- aggregated_preferences[pref_column] + percentile_tolerance
      suitable_movies <- suitable_movies %>% 
        filter(
          !!sym(pref_column) <= upper_bound[[1]],
          !!sym(pref_column) >= lower_bound[[1]]
        )

    }

  }


  
  # Check if there are suitable movies
  if (nrow(suitable_movies) == 0) {
    print("No suitable movies found based on user preferences.")
    return(NULL)
  }
  
  # Calculate cosine similarity between user preferences and movie features
  similarity_scores <- sapply(1:nrow(suitable_movies), function(i) {
    movie_vector <- as.numeric(suitable_movies[i, focus_columns])
    cosine_similarity(aggregated_preferences, movie_vector)
  })
  
  # Recommend top k movies based on similarity scores
  top_recommendations <- suitable_movies[order(similarity_scores, decreasing = TRUE), ]
  top_recommendations$similarity_score = similarity_scores
  return(top_recommendations)
}




# Step 3: Compute Similarity
# Simulate preferences for each user
num_users <- 100
user_data <- replicate(num_users, simulate_user_preferences())

# Convert simulated user preferences to dataframe
user_df <- as.data.frame(t(user_data))
colnames(user_df) <- user_pref_columns

# Add user ID column
user_df$user_id <- 1:num_users

# Compute similarity matrix between users
similarity_matrix <- matrix(NA, nrow = num_users, ncol = num_users)
for (i in 1:num_users) {
  for (j in 1:num_users) {
    similarity_matrix[i, j] <- cosine_similarity(user_df[i, ], user_df[j, ])
  }
}

# Scale similarity scores to a range of 0 to 1
similarity_matrix <- (similarity_matrix - min(similarity_matrix, na.rm = TRUE)) / (max(similarity_matrix, na.rm = TRUE) - min(similarity_matrix, na.rm = TRUE))

# Print similarity matrix
print(similarity_matrix)

# Example: Make recommendations for user 1
user_id <- 33
recommendations <- make_recommendations(user_id, k = 5)
