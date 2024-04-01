# Load movie data
movies <- read.csv("movie_data.csv")  # Assuming CSV file name is "movies_data.csv"

# Convert genres to uppercase
movies$genres <- toupper(movies$genres)
movies$production_companies <- toupper(movies$production_companies)

# One-hot encode genres
genres <- unique(unlist(strsplit(movies$genres, ", ")))
for (genre in genres) {
  movies[paste0("GENRE_", genre)] <- grepl(genre, movies$genres)
}

# Extract production company names and split them
production_companies <- strsplit(as.character(movies$production_companies), ",\\s*")

# Flatten the list of lists into a single vector
production_companies <- unlist(production_companies)

# Get counts of production companies
production_counts <- table(production_companies)

# Sort the counts in descending order
production_counts <- sort(production_counts, decreasing = TRUE)

top_100_production <- names(production_counts)[1:100]
for (comp in top_100_production) {
  movies[paste0("COMP_", comp)] <- grepl(comp, movies$production_companies)
}


movies$release_year = as.numeric(substr(movies$release_date, 1, 4))
define_generation <- function(years) {
  generations <- character(length(years))
  
  for (i in seq_along(years)) {
    if (years[i] >= 2010) {
      generations[i] <- paste0("2010's")
    } else if (years[i] >= 2000) {
      generations[i] <- paste0("2000's")
    } else if (years[i] >= 1990) {
      generations[i] <- paste0("1990's")
    } else if (years[i] >= 1980) {
      generations[i] <- paste0("1980's")
    } else if (years[i] >= 1970) {
      generations[i] <- paste0("1970's")
    } else if (years[i] >= 1960) {
      generations[i] <- paste0("1960's")
    } else if (years[i] >= 1950) {
      generations[i] <- paste0("1950's")
    } else {
      generations[i] <- paste0("Before 1950's")
    }
  }
  
  return(generations)
}

movies$generation = define_generation(movies$release_year)
generation <- unique(unlist(strsplit(movies$generation, ", ")))
for (gen in generation) {
  movies[paste0("DECADE_", gen)] <- grepl(gen, movies$generation)
}

