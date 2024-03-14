library(rvest)
library(httr)
library(dplyr)
library(tidyverse)
library(tidyr)

movie_data = read.csv('movie_data.csv')
movie_data$spoken_languages = toupper(movie_data$spoken_languages)
movie_data = movie_data %>% filter(grepl('ENGLISH', spoken_languages))
movie_data_init_features = movie_data %>% select(
  tmdb_id, movie_title, genres, overview, year_released, runtime, vote_average, vote_count, spoken_languages, production_countries
)

movies = movie_data_init_features %>% unique() %>% filter(genres != "[]", production_countries != "[]")
movies$genres <- sapply(movies$genres, FUN = fromJSON)
movies$spoken_languages <- sapply(movies$spoken_languages, FUN = fromJSON)
movies$production_countries <- sapply(movies$production_countries, FUN = fromJSON)

one_hot_encode_columns = function(df, column_name){
  for (row in 1:nrow(df)){
    print(row)
    df_sliced = df %>% slice(row)
    row_value = df_sliced[[column_name]]
    for (value in row_value){
      upper_column_name = toupper(value)
      final_column_name = paste0("is_",upper_column_name)
      final_column_name <- str_replace_all(final_column_name, "\\s", "")
      if(length(final_column_name) > 1){
        for (name in final_column_name){
          if (!(name %in% names(df))){
            df[[name]] = 0
            df[row, name] <- 1
            
            
          }
          else{
            df[row, name] <- 1
            
          }
        }
      }
      else{
        if (!(final_column_name %in% names(df))){
          df[[final_column_name]] = 0
          df[row, final_column_name] <- 1
          
          
        }
        else{
          df[row, final_column_name] <- 1
          
        }
      }
    }
  }
  return(df)
}

test_encoding = one_hot_encode_columns(movies, 'genres')
test_encoding = one_hot_encode_columns(test_encoding, 'production_countries')
