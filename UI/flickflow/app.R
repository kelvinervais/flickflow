# Imports --------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(dplyr)

# Functions --------------------------------------------------------------------
# Function to get poster URL for a movie given its TMDb ID
get_movie_poster_url <- function(tmdb_id, api_key) {
  # Base URL for TMDb API
  base_url <- "https://api.themoviedb.org/3"
  
  # Endpoint for movie details
  endpoint <- paste0("/movie/", tmdb_id)
  
  # Make the request to TMDb API
  response <- httr::GET(url = paste0(base_url, endpoint),
                        query = list(api_key = api_key))
  
  # Check if the request was successful
  if (httr::http_status(response)$category == "Success") {
    # Extract poster path
    data <- httr::content(response)
    poster_path <- data$poster_path
    
    if (!is.null(poster_path)) {
      # Construct full URL for the poster
      poster_url <- paste0("https://image.tmdb.org/t/p/original", poster_path)
      
      return(poster_url)
    } else {
      message("No poster available for this movie.")
      return(NULL)
    }
  } else {
    message("Failed to retrieve movie details. Check your API key and TMDb ID.")
    return(NULL)
  }
}


movie_data = read.csv('movie_data.csv')
movie_data$spoken_languages = toupper(movie_data$spoken_languages)
movie_data = movie_data %>% filter(grepl('ENGLISH', spoken_languages)) %>% arrange(desc(popularity))

reviews = read.csv("scraped_reviews_ratings.csv")
# Define UI for application that draws a histogram
# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Sidebar Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("My Movies", tabName = "movies", icon = icon("film")),
      menuItem("My Ratings", tabName = "ratings", icon = icon("star")),
      menuItem("Settings", tabName = "settings", icon = icon("gear"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home", h2("Welcome to Home")),
      tabItem(tabName = "movies", h2("My Movies")),
      tabItem(tabName = "ratings", h2("My Ratings")),
      tabItem(tabName = "settings", h2("Settings"))
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
