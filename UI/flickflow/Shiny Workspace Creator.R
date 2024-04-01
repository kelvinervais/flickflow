library(dplyr)



movie_data = read.csv('/Users/kelvinervais/Desktop/flickflow/UI/flickflow-workspace/movie_data.csv')
movie_data$spoken_languages = toupper(movie_data$spoken_languages)
movie_data = movie_data %>% filter(grepl('ENGLISH', spoken_languages)) %>% arrange(desc(popularity))

reviews = read.csv("/Users/kelvinervais/Desktop/flickflow/UI/flickflow-workspace/scraped_reviews_ratings.csv")






save(movie_data,reviews, file = "~/Desktop/flickflow/UI/flickflow/flickflow_base_workspace.RData")
