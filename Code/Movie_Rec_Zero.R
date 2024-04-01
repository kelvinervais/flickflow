library(dplyr)
# Load movie data
movies <- read.csv("movie_data.csv")  



# Simulate user inputs
user_prefs <- list(
  top_genres = c("THRILLER", "DRAMA", "COMEDY"),
  ideal_runtime = 110,
  generation_prefs = "2010's"
)



# Function to filter movies based on user preferences
filter_movies <- function(movies, user_prefs) {
  # Filter by genres
  selected_genres <- toupper(user_prefs$top_genres)
  filter_condition <- rowSums(movies[, paste0("GENRE_", selected_genres)]) > 0
  filtered_movies <- movies[filter_condition, ]
  
  # Filter by runtime

  filtered_movies <- filtered_movies[filtered_movies$runtime >= user_prefs$ideal_runtime - 20, ]

  filtered_movies <- filtered_movies[filtered_movies$runtime < user_prefs$ideal_runtime + 20,, ]
  
  
  # Filter by generation preferences
  filtered_movies <- filtered_movies[grepl(user_prefs$generation_prefs, filtered_movies$generation), ]
  
  return(filtered_movies)
}

# Function to rank movies
rank_movies <- function(filtered_movies) {
  ranked_movies <- filtered_movies[order(filtered_movies$popularity, decreasing = TRUE), ]
  
  return(ranked_movies)
}

# Function to recommend top N movies
recommend_movies <- function(ranked_movies, N = 10) {
  ranked_movies = ranked_movies %>% distinct(production_companies, .keep_all = TRUE)
  ranked_movies = ranked_movies %>% distinct(genres, .keep_all = TRUE)
  
  top_N_movies <- head(ranked_movies, N)
  return(top_N_movies)
}

# Function to present recommended movies and get user selection
present_recommendations <- function(recommended_movies) {
  cat("Here are your recommended movies:\n")
  for (i in 1:nrow(recommended_movies)) {
    cat(i, ". ", recommended_movies[i, "title"], " (", recommended_movies[i, "release_year"], ")\n", sep = "")
  }
  cat("\n")
  selection <- as.integer(readline(prompt = "Please select a movie from the list: "))
  return(selection)
}

# Filter movies based on user preferences
filtered_movies <- filter_movies(movies, user_prefs)

# Rank filtered movies
ranked_movies <- rank_movies(filtered_movies)

# Recommend top N movies
recommended_movies <- recommend_movies(ranked_movies, N = 3)

# Display recommended movies and get user selection
selected_index <- present_recommendations(recommended_movies)
initial_recommendation <- recommended_movies[selected_index, ]

# Display the user's initial recommendation
cat("\nYour initial recommendation is:\n", initial_recommendation$title, "(", initial_recommendation$release_year, ")\n")


# Function to find similar movies based on genres
find_similar_movies <- function(initial_genres, movies) {
  initial_genres <- unlist(strsplit(initial_genres, ", "))
  similar_movies <- movies[sapply(movies$genres, function(movie_genres) any(grepl(paste(initial_genres, collapse = "|"), movie_genres))), ]
  return(similar_movies)
}

# Find similar movies based on genres
similar_movies <- find_similar_movies(initial_recommendation$genres, movies)
similar_movies <- similar_movies[!similar_movies$id %in% initial_recommendation$id, ]  # Remove the initial recommendation


# Content filtering: Find movies with similar popularity and vote counts
content_filtered <- similar_movies[similar_movies$vote_average >= initial_recommendation$vote_average - 1.5 & 
                                     similar_movies$vote_average <= initial_recommendation$vote_average + 1.5, ]

content_filtered <- content_filtered[!content_filtered$id %in% initial_recommendation$id, ]  # Remove the initial recommendation

# Take top 50 unique recommendations
top_50_recommendations <- unique(head(content_filtered, 50))

# Display the recommendations
print(top_50_recommendations)
