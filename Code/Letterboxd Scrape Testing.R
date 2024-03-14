library(rvest)
library(httr)
library(dplyr)

# Function to scrape reviews and star ratings for a given movie URL
scrape_letterboxd_reviews <- function(movie_url, num_pages = 5) {
  # Initialize lists to store reviews and star ratings
  all_reviews <- list()
  all_ratings <- list()
  
  # Iterate over pages
  for (page in 1:num_pages) {
    # Construct URL for the movie reviews page
    url <- paste0(movie_url, "reviews/by/activity/page/", page, "/")
    
    # Send HTTP request
    response <- GET(url)
    
    # Check if request was successful
    if (status_code(response) == 200) {
      # Parse HTML content
      webpage <- read_html(content(response, "text"))
      
      # Extract review elements
      review_elements <- webpage %>%
        html_nodes(".film-detail-content")
      
      # Check if there are review elements
      if (length(review_elements) > 0) {
        # Iterate through review elements on the current page
        for (i in 1:length(review_elements)) {
          review_text_node <- review_elements[[i]] %>%
            html_node(".body-text")
          rating_node <- review_elements[[i]] %>%
            html_node(".rating")
          
          # Extract review text and star rating
          if (!is.null(review_text_node) && !is.null(rating_node)) {
            review_text <- html_text(review_text_node)
            rating <- html_text(rating_node)
            
            # Append review and rating to respective lists
            all_reviews[[length(all_reviews) + 1]] <- review_text
            all_ratings[[length(all_ratings) + 1]] <- rating
          } else {
            cat("Error: Review or star rating not found for review", i, "on page", page, "\n")
          }
        }
      } else {
        cat("No review elements found on page", page, "\n")
      }
    } else {
      cat("Failed to retrieve page", page, ". HTTP status code:", status_code(response), "\n")
    }
  }
  
  return(list(reviews = all_reviews, ratings = all_ratings))
}


movie_data = read.csv('movie_data.csv')
movie_data$spoken_languages = toupper(movie_data$spoken_languages)
movie_data = movie_data %>% filter(grepl('ENGLISH', spoken_languages)) %>% arrange(desc(popularity))
# # Define a list of movie IDs
movie_ids <- movie_data$movie_id

# Initialize an empty dataframe
reviews_df <- data.frame(movie_id = character(), review = character(), rating = character(), stringsAsFactors = FALSE)

# Iterate over each movie URL
for (i in 1:5) {
  print(i)
  movie_url <- movie_urls[i]
  # Scrape reviews and ratings for the current movie URL
  scraped_data <- scrape_letterboxd_reviews(movie_url, num_pages = 5)
  reviews <- scraped_data$reviews
  ratings <- scraped_data$ratings
  
  # Create a dataframe for the current movie URL
  movie_reviews_df <- data.frame(movie_id = rep(movie_data$movie_id[i], length(reviews)),
                                 review = unlist(reviews),
                                 rating = unlist(ratings),
                                 stringsAsFactors = FALSE)
  
  # Combine the dataframe with the main dataframe
  reviews_df <- bind_rows(reviews_df, movie_reviews_df)
}


write.csv(reviews_df, 'scraped_reviews_ratings.csv')


#df = read.csv('scraped_reviews_ratings.csv')
