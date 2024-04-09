app_type = 'prod'
# IMPORTS --------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinyauthr)
library(dplyr)
library(shinyjs)
library(shinyWidgets)
library(shinyBS)
library(shinyalert)
library(uuid)
library(mongolite)
library(shinysense)
library(httr)
library(jsonlite)
library(shinyMobile)
library(DT)
#preview_mobile(appPath = system.file("~/Desktop/flickflow/UI/flickflow/app.R", package = "shinyMobile"), device = "iphoneX")
#R -e "shiny::runApp('~/Desktop/flickflow/UI/flickflow/app.R', port = 3838)"
admin_accounts = c('kervais')

if (app_type == 'dev'){
  loaded_objects <- readRDS("~/Desktop/flickflow/UI/flickflow/flickflow_base_workspace.rds")

  
}
if (app_type == 'prod'){
  loaded_objects <- readRDS("flickflow_base_workspace.rds")
}

movies <- loaded_objects$movies
similarity_matrix <- loaded_objects$similarity_matrix
user_df = loaded_objects$user_df
user_pref_columns <- c(grep("^(GENRE_|DECADE_|COMP_|.*_percentile)", names(user_df), value = TRUE))


genre_options = unique(unlist(strsplit(movies$genres, ", ")))

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
  
  
  mongo_database$update(paste0('{"entry_type": "user_profile",
                           "username": "',user,'"}'),
                        paste0('{"$set":', toJSON_updater(upsert_content), '}'),
                        upsert = TRUE)
  
}






# TMDB FUNCS --------------------------------------------------------------------


# Function to get movie details
get_movie_info <- function(movie_id) {
  url <- paste0("https://api.themoviedb.org/3/movie/", movie_id, "?api_key=", api_key)
  if(length(url) != 1){
    return()
  }
  response <- GET(url)
  if (http_status(response)$category == "Success") {
    movie_info <- content(response, as = "text") %>%
      fromJSON()
    return(movie_info)
  } else {
    return(NULL)
  }
}

get_movie_streaming <- function(movie_id) {
  url <- paste0("https://api.themoviedb.org/3/movie/", movie_id, "/watch/providers", "?api_key=", api_key)
  if(length(url) != 1){
    return()
  }
  response <- GET(url)
  if (http_status(response)$category == "Success") {
    movie_info <- content(response, as = "text") %>%
      fromJSON()
    return(movie_info)
  } else {
    return(NULL)
  }
}



# Function to get movie credits
get_movie_credits <- function(movie_id) {
  url <- paste0("https://api.themoviedb.org/3/movie/", movie_id, "/credits?api_key=", api_key)
  response <- GET(url)
  
  if (http_status(response)$category == "Success") {
    credits <- content(response, as = "text") %>%
      fromJSON()
    
    director <- credits$crew[credits$crew$known_for_department == "Directing", "name"][1]
    cast <- credits$cast[1:5, "name"]
    
    return(list(director = director, cast = cast))
  } else {
    return(NULL)
  }
}

# Function to get movie poster
get_movie_poster <- function(movie_info) {
  if (!is.null(movie_info$poster_path)) {
    poster_url <- paste0("https://image.tmdb.org/t/p/original", movie_info$poster_path)
    return(poster_url)
  } else {
    return(NULL)
  }
}

# Set your TMDb API key
api_key <- "49a0e80c2bb018a1ca19333c483c2aac"



# INITIAL RECS --------------------------------------------------------------------
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
  gen_column = paste0('DECADE_',user_prefs$generation)

  filtered_movies <- filtered_movies %>%
    filter(!!sym(gen_column) == TRUE)   
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


# Function to find similar movies based on genres
find_similar_movies <- function(initial_genres, movies) {
  initial_genres <- unlist(strsplit(initial_genres, ", "))
  similar_movies <- movies[sapply(movies$genres, function(movie_genres) any(grepl(paste(initial_genres, collapse = "|"), movie_genres))), ]
  return(similar_movies)
}





# COLLAB FILTERING RECS --------------------------------------------------------------------

# Function to compute cosine similarity between two vectors
cosine_similarity <- function(x, y) {
  dot_product <- sum(x * y)
  magnitude_x <- sqrt(sum(x^2))
  magnitude_y <- sqrt(sum(y^2))
  similarity <- dot_product / (magnitude_x * magnitude_y)
  return(similarity)
}

# Function to make recommendations for a user based on cosine similarity
make_recommendations <- function(user_id, k = 5, percentile_tolerance = 0.2, movies_to_filter) {
  # Find top k similar users to the given user (excluding the user itself)
  user_index <- which(rownames(similarity_matrix) == user_id)
  
  similar_users <- order(similarity_matrix[user_index, ], decreasing = TRUE)[-1][1:k]
  
  # Check if there are similar users
  if (length(similar_users) == 0) {
    print("No similar users found. Unable to make recommendations.")
    return(NULL)
  }
  user_pref_columns <- c(grep("^(GENRE_|DECADE_|COMP_|.*_percentile)", names(user_df), value = TRUE))
  user_pref_columns <- gsub("'", "_", user_pref_columns)
  user_pref_columns <- gsub(" ", "_", user_pref_columns)
  user_pref_columns <- gsub("\\.", "_", user_pref_columns)
  # Aggregate preferences of similar users
  aggregated_preferences <- colMeans(user_df[similar_users, user_pref_columns], na.rm = TRUE)
  
  # Filter aggregated preferences to include only columns where mean value is greater than 0
  aggregated_preferences <- aggregated_preferences[aggregated_preferences > 0]  
  focus_columns = names(aggregated_preferences)
  # Filter movies to include only those that fit user preferences
  suitable_movies <- movies %>% filter(!(id %in% movies_to_filter))
  fix_movie_names = names(suitable_movies)
  fix_movie_names <- gsub("'", "_", fix_movie_names)
  fix_movie_names <- gsub(" ", "_", fix_movie_names)
  fix_movie_names <- gsub("\\.", "_", fix_movie_names)
  names(suitable_movies) = fix_movie_names
  top_genre <- names(sort(aggregated_preferences[grep("GENRE_", names(aggregated_preferences))], decreasing = TRUE))[1:3]
  genre_1 = ifelse(is.na(top_genre[1]), " ",top_genre[1])
  genre_2 = ifelse(is.na(top_genre[2]), " ",top_genre[2])
  genre_3 = ifelse(is.na(top_genre[3]), " ",top_genre[3])
  
  suitable_movies <- suitable_movies %>% filter(
    !!sym(genre_1) == T |
      !!sym(genre_2) == T |
      !!sym(genre_3) == T
  )
  
  top_company <- names(sort(aggregated_preferences[grep("COMP_", names(aggregated_preferences))], decreasing = TRUE))[1:3]
  comp_1 = ifelse(is.na(top_company[1]), "",top_company[1])
  comp_2 = ifelse(is.na(top_company[2]), comp_1,top_company[2])
  comp_3 = ifelse(is.na(top_company[3]), comp_2,top_company[3])
  suitable_movies <- suitable_movies %>%   filter(
    if (!is.na(comp_1)) (!!sym(comp_1)) else FALSE |
      if (!is.na(comp_2)) (!!sym(comp_2)) else FALSE |
      if (!is.na(comp_3)) (!!sym(comp_3)) else FALSE
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
  top_recommendations = top_recommendations %>% arrange(desc(similarity_score))
  return(top_recommendations)
}
# UI --------------------------------------------------------------------

value_box_fixed = function (value, subtitle, icon, color = "aqua", width = 4, 
          href = NULL) 
{
  if (!is.null(icon)) 
    #tagAssert(icon, type = "i")
  boxContent <- div(class = paste0("small-box bg-", color), 
                    div(class = "inner", h3(value), p(subtitle)), if (!is.null(icon)) 
                      div(class = "icon-large", icon))
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  div(class = if (!is.null(width)) 
    paste0("col-sm-", width), boxContent)
}

renderValueBox_fixed = function (expr, env = parent.frame(), quoted = FALSE) 
{
  vbox_fun <- shiny::exprToFunction(expr, env, quoted)
  shiny::renderUI({
    vbox <- vbox_fun()
    vbox$children[[1]]
    
  })
}


ui <- dashboardPage(
  # ...HEADER --------------------------------------------------------------------
  
  # put the shinyauthr logout ui module in here
  dashboardHeader(
    title = "FlickFlow",
    tags$li(class = "dropdown", style = "padding: 8px;", shinyauthr::logoutUI("logout"))
  ),
  
  # ...SIDEBAR --------------------------------------------------------------------
  
  # setup a sidebar menu to be rendered server-side
  dashboardSidebar(
    collapsed = FALSE,
    sidebarMenu(id="tabs",
                sidebarMenuOutput("sidebarUI")
    )
  ),
  # ... BODY --------------------------------------------------------------------
  
  dashboardBody(
    shinyjs::useShinyjs(),
    
    # put the shinyauthr login ui module here
    shinyauthr::loginUI("login", 
                        title = 'Please login/signup to access FlickFlow!',
                        error_message = 'No account found, please sign up!',
                        additional_ui = shiny::tagList(
                          uiOutput('signupUI', )
                        )
    ),
    uiOutput('bodyUI'),
    conditionalPanel(
      condition = "input.tabs == 'home'",
       shinyswipr_UI( "SwiprModel", 
                      fluidRow(
                        column(12,
                               align = 'center',
                               uiOutput('homeRecDisplay') 
                               )
  
            
              
       )
     )
                   
    ),
    conditionalPanel(
      condition = "input.tabs == 'movies'",
      fluidRow(
        valueBoxOutput('flickCounter'),
        valueBoxOutput('reviewCounter'),
        valueBoxOutput('genreCounter')
        
      ),
      fluidRow(
             DTOutput('myMoviesTable')
             )

      
    )
  )
)

# SERVER --------------------------------------------------------------------

server <- function(input, output, session) {
  # ... BACKING DATA --------------------------------------------------------------------
  
  mongo_backing_reactive <- reactiveVal(mongo_database$find())
  
  
  user_base_reactive = reactive({
    init_mongo <- mongo_database$find()
    
    user_base <- tibble::tibble(
      user = init_mongo$username,
      password = sapply(init_mongo$password, sodium::password_store),
      permissions = ifelse(init_mongo$username %in% admin_accounts, 'admin', 'standard'),
      name = init_mongo$username
    )
    user_base
  })
  
  observe({
    credentials <<- shinyauthr::loginServer(
      id = "login",
      data = user_base_reactive(),
      user_col = user,
      pwd_col = password,
      sodium_hashed = TRUE,
      log_out = reactive(logout_init()),
      
    )
  })  

    # ...... SIGNUP REACTIVE --------------------------------------------------------------------
    
  observeEvent(input$signupConfirmation, {
    
    signup_usename = trimws(input$signupUsername)
    signup_password= trimws(input$signupPassword)
    if (length(input$signupGenrePicks) < 3){
      shinyalert("Please select 3 genres", type = "error")
      
    }
    genres = input$signupGenrePicks
    runtime = input$signupRuntime
    
    guid = UUIDgenerate()
    date_added = Sys.Date()
    mongo_df = data.frame(
      entry_type = 'user_profile', #Can be types User Profile, User Entries
      guidUserId =  guid, #Global identifier for users, will be generated upon account creation and for any interaction
      username = signup_usename, #username for account, only under type User Profile
      password = signup_password, #password for account, only under type User Profile
      date_joined = date_added, #date account created, only under type User Profile
      flicks_right = 0, #Running number of flicks right, only under type User Profile
      flicks_left = 0, #Running number of flicks left, only under type User Profile,
      total_flicks = 0,
      movies_flicked = c('Init'), #Running list of movie ids that were flicked, only under type User Profile
      modelStatus = c('Zero'),
      #User My Movies Columns
      movies_right = c('Init'), #Running list of movie ids that were flicked right, only under type User Profile
      movies_left = c('Init') #Running list of movie ids that were flicked left, only under type User Profile
    )
    
    user_pref_columns <- c(grep("^(GENRE_|DECADE_|COMP_|.*_percentile)", names(movies), value = TRUE))
    columns_to_add = data.frame(matrix(ncol = length(user_pref_columns), nrow = 1))
    names(columns_to_add) = user_pref_columns
    columns_to_add[is.na(columns_to_add)] <- 0 
    genres = ifelse(genres == 'SCIENCE FICTION', 'SCIENCE.FICTION', genres)
    genres = ifelse(genres == 'TV MOVIE', 'TV.MOVIE', genres)
    generation <- tolower(input$signupGeneration)
    generation <- gsub("'", ".", generation)
    generation <- gsub(" ", ".", generation)
    
    for (genre in genres){
      if (paste0('GENRE_',genre) %in% names(columns_to_add)){
        columns_to_add[[paste0('GENRE_',genre)]] = 1
        
      }
    }
    
    columns_to_add[[paste0('DECADE_',generation)]] = 1
    
    runtime_percentile_of_pref = runtime
    columns_to_add$runtime_percentile = runtime_percentile_of_pref
    
    
    mongo_df = data.frame(cbind(mongo_df, columns_to_add))
    
    initial_pick = input$signupMoviePicks
    movie_name <- sub("\\s*\\(\\d+\\)", "", initial_pick)
    year <- sub(".*\\((\\d+)\\)", "\\1", initial_pick)
    initial_recommendation = movies %>% filter(release_year == year, grepl(movie_name, title))
    
    # mongo_df$popularity_percentile = initial_recommendation$popularity
    # mongo_df$vote_average_percentile = initial_recommendation$vote_average
    # mongo_df$revenue_percentile = initial_recommendation$revenue
    # mongo_df$budget_percentile = initial_recommendation$budget
    
    mongo_df$movies_flicked = initial_recommendation$id
    
    
    similar_movies <- find_similar_movies(initial_recommendation$genres, movies)
    similar_movies <- similar_movies[!similar_movies$id %in% initial_recommendation$id, ]  # Remove the initial recommendation
    
    
    # Content filtering: Find movies with similar popularity and vote counts
    content_filtered <- similar_movies[similar_movies$vote_average >= initial_recommendation$vote_average - 1.5 & 
                                         similar_movies$vote_average <= initial_recommendation$vote_average + 1.5, ]
    
    content_filtered <- content_filtered[!content_filtered$id %in% initial_recommendation$id, ]  # Remove the initial recommendation
    
    # Take top 50 unique recommendations
    top_50_recommendations <- unique(head(content_filtered, 50))
    first_50_ids = list(top_50_recommendations$id)
    mongo_df$zero_state_recs = first_50_ids
    mongo_database$insert(mongo_df)
    
    
    removeModal()
    
    session$reload()
    
  })
  # ...... CARD REACTIVE --------------------------------------------------------------------
  
  card_swipe_zero<- callModule(shinyswipr, "SwiprModel")
  
  observeEvent(card_swipe_zero(),{
    direction = card_swipe_zero()
    if(!(direction %in% c('left','right'))){
      shinyalert("Please swipe left or right only", type = "warning")
      movie_being_displayed(movie_for_upload()$id)
      return()
      
      
    }
    user_to_display = credentials()$info$user
    user_backend = mongo_backing_reactive() %>% filter(entry_type == 'user_profile',username == user_to_display)
    movie_swiped = movie_for_upload()
    user_true_false_columns <- c(grep("^(GENRE_|DECADE_|COMP_)", names(movie_swiped), value = TRUE))

    filtered_df <- movie_swiped[, user_true_false_columns][, colSums(movie_swiped[, user_true_false_columns]) == nrow(movie_swiped)]
    column_names_fixed <- gsub("'", "_", names(filtered_df))
    column_names_fixed <- gsub(" ", "_", column_names_fixed)
    column_names_fixed <- gsub("\\.", "_", column_names_fixed)
    
    user_percentile_columns <- c(grep("^(.*_percentile)", names(movie_swiped), value = TRUE))

    movies_swiped_so_far = movies %>% filter(id %in% c(user_backend$movies_flicked, movie_swiped$id))
    user_backend_binary_columns = user_backend %>% select(column_names_fixed)
    user_backend_binary_columns = user_backend_binary_columns +1
    user_backend_mean_columns = user_backend %>% select(user_percentile_columns)
    
    user_backend_upsert_columns = cbind(user_backend_binary_columns, user_backend_mean_columns)
    
    
    if(direction == 'left'){
      user_backend_upsert_columns$flicks_left = user_backend$flicks_left + 1
      user_backend_upsert_columns$movies_left = list(append(unlist(user_backend$movies_left),movie_swiped$id))
      
    }
    if(direction == 'right'){
      user_backend_upsert_columns$flicks_right = user_backend$flicks_right + 1
      user_backend_upsert_columns$movies_right = list(append(unlist(user_backend$movies_right),movie_swiped$id))
    }
    user_backend_upsert_columns$total_flicks = user_backend$total_flicks + 1
    
    if(user_backend$total_flicks + 1 == 25){
      user_backend_upsert_columns$modelStatus = 'Base'
      user_init_recs = unlist(user_backend$zero_state_recs)
      user_init_recs <- user_init_recs[user_init_recs != movie_swiped$id ]
      user_backend_upsert_columns$base_state_recs = list(user_init_recs)
      

    }
    user_backend_upsert_columns$movies_flicked =  list(append(unlist(user_backend$movies_flicked),movie_swiped$id))
    
    user_backend_upsert_columns$popularity_percentile = mean(movies_swiped_so_far$popularity)
    user_backend_upsert_columns$vote_average_percentile = mean(movies_swiped_so_far$vote_average)
    user_backend_upsert_columns$revenue_percentile = mean(movies_swiped_so_far$revenue)
    user_backend_upsert_columns$budget_percentile = mean(movies_swiped_so_far$budget)
    

    


    if (user_backend$modelStatus == 'Zero'){
      user_init_recs = unlist(user_backend$zero_state_recs)
      user_init_recs <- user_init_recs[user_init_recs != movie_swiped$id ]
      user_backend_upsert_columns$zero_state_recs = list(user_init_recs)
      
      movie_to_display = sample(user_init_recs, 1)
      movie_being_displayed(movie_to_display)
    }
    
    if (user_backend$modelStatus == 'Base'){
      user_movies_to_filter = user_backend_upsert_columns$movies_flicked
      
      user_to_pull = user_backend$username
      if(!(user_to_pull %in% user_df$username)){
        display_user = user_backend %>% select(username, all_of(user_pref_columns))
        distances <- as.matrix(dist(rbind(display_user, user_df))) # Calculate distances
        similar_row_index <- which.min(distances[nrow(display_user),]) # Find the index of the most similar row
        sim_user = user_df[similar_row_index,]
        user_to_pull = sim_user
        
      }
      recommendations <- make_recommendations(user_to_pull, k = 5, movies_to_filter = user_movies_to_filter)
      recommendations = recommendations$id
      recommendations = recommendations[recommendations != movie_swiped$id ]
      
      user_backend_upsert_columns$base_state_recs = list(recommendations)
      
      movie_to_display = sample(recommendations, 1)
      movie_being_displayed(movie_to_display)
    }
    
    upsert_data(user_backend_upsert_columns, user_backend$username)
    
    mongo_backing_reactive(mongo_database$find())
    
  })

    

    
  # ... CREDENTIALS --------------------------------------------------------------------
  
  
  # Logout to hide
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  sidebar_initialized = reactiveVal({NULL})
  
  # ... SIGNUP UI --------------------------------------------------------------------
  
  output$signupUI <- renderUI({
    sidebar_initialized(TRUE)
    fluidRow(
      column(width = 12, align = "center", 
             renderText('First time user?  Please sign up to get Flicking!'),
             actionBttn('signupButton', 'Sign Up', size = 'sm')
      )
    )
  })
  
  observeEvent(input$signupButton, {
    showModal(
      modalDialog(
        title = "Sign Up for FlickFlow!",
        uiOutput('signupFormUI')
      )
    )
  })
  
  output$signupPostUsername = renderUI({
    if (input$signupUsername == ''){
      return()
    }
    
    if((trimws(input$signupUsername) %in% user_base_reactive()$user == T)){
      return(
        h1('Username already exists, please try another')
      )
    }


    fluidRow(
      passwordInput('signupPassword', 'Pick a password')
      
      
    )
    
  })
  output$signupPostPassword= renderUI({
    if (is.null(input$signupPassword)){
      return()
    }
    if (input$signupPassword == ''){
      return()
    }
    

    fluidRow(
      selectizeInput(
        "signupGenrePicks",
        label = "Please share your 3 favorite genres",
        choices = genre_options,
        multiple = TRUE,
        options = list(maxItems = 3)
      ),
      sliderInput('signupRuntime', 'Please share your ideal movie runtime (Minutes)', min = 30, max = 200, value = 115, step = 5, ticks = T),
      sliderTextInput('signupGeneration', label = "What is your 'Golden Age' of movies?",
                      choices = c("Before 1950's", "1950's", "1960's", "1970's", "1980's","1990's","2000's", "2010's"), grid = T, selected  ="1980's" 
      ),

      
    )
    
  })
  
  output$signupQuizMovies = renderUI({
    if(is.null(input$signupGenrePicks)){
      return()
    }
    if(length(input$signupGenrePicks) != 3){
      return()
    }
    generation = input$signupGeneration
    generation <- tolower(generation)
    generation <- gsub("'", ".", generation)
    generation <- gsub(" ", ".", generation)
    
    genres = input$signupGenrePicks
    genres = ifelse(genres == 'SCIENCE FICTION', 'SCIENCE.FICTION', genres)
    genres = ifelse(genres == 'TV MOVIE', 'TV.MOVIE', genres)
    user_prefs <- list(
      top_genres = genres,
      ideal_runtime = input$signupRuntime,
      generation_prefs = generation
    )
    filtered_movies <- filter_movies(movies, user_prefs)
    
    # Rank filtered movies
    ranked_movies <- rank_movies(filtered_movies)
    
    # Recommend top N movies
    recommended_movies <- recommend_movies(ranked_movies, N = 3)
    movie_titles <- recommended_movies$title
    
    movies_plus_year = paste0(movie_titles, " (", recommended_movies$release_year, ")")
    selectizeInput(
      "signupMoviePicks",
      label = "Please pick your favorite of these three movies",
      choices = movies_plus_year,
      multiple = F
    )
    
    
  })
  
  output$finalSignupButton <- renderUI({
    if(is.null(input$signupMoviePicks)){
      return()
    }
    fluidRow(
      actionBttn('signupConfirmation', 'Sign Up?', size = 'sm')
      
      )
  })
  
  output$signupFormUI <- renderUI({
    fluidRow(
      column(width = 12, align = "center", 
             textInput('signupUsername', label = 'Pick a username'),
             uiOutput('signupPostUsername'),
             uiOutput('signupPostPassword'),
             uiOutput('signupQuizMovies'),
             uiOutput('finalSignupButton')
             
      )
    )
  })

  # ... BODY UI --------------------------------------------------------------------
  
  output$bodyUI <- renderUI({
    
    
    tabItems(
      tabItem(tabName = "home",
              p("Welcome to FlickFlow, a people-driven movie recommendation engine!"),
              p("Swipe Left if you Dislike, Swipe Right if you Like!"),
              ),
      tabItem(tabName = "movies", 
              h2("My Flicks"),
              p("Welcome to MyFlicks!  Here you can see your past swipes and learn more about your swipes"),
              ),
      tabItem(tabName = "ratings", h2("My Ratings")),
      tabItem(tabName = "settings", h2("Settings"))
    )    
  })
  
  
  # ...... HOME UI --------------------------------------------------------------------
  
  movie_for_upload = reactiveVal(NULL)
  
  movie_being_displayed = reactiveVal(NULL)
  
  output$homeRecDisplay = renderUI({
    if(credentials()$user_auth == F){
      return()
    }  
    user_to_display = credentials()$info$user
    user_backend = mongo_backing_reactive() %>% filter(entry_type == 'user_profile',username == user_to_display)
    if (user_backend$modelStatus == 'Zero'){
      
      if(is.null(movie_being_displayed())){
        user_init_recs = unlist(user_backend$zero_state_recs)
        movie_to_display = sample(user_init_recs, 1)
        movie_data_to_display_zero = movies %>% filter(id == movie_to_display)
        movie_for_upload(movie_data_to_display_zero)
      }
      else{
        movie_to_display = movie_being_displayed()
        movie_data_to_display_zero = movies %>% filter(id == movie_to_display)
        movie_for_upload(movie_data_to_display_zero)
        
      }
      
      
      
      
      
      movie_title = paste(movie_data_to_display_zero$title, "-", movie_data_to_display_zero$release_year)
      print(movie_title)
      # Get movie information
      movie_info <- get_movie_info(movie_data_to_display_zero$id)
      
      if (is.null(movie_info)) {
        return(
          fluidRow(
            h1(movie_title),
            div(id = "myImageBlank",
                tags$img(src = 'test_logo.png', style = "width: 300px; height: 500px;")
            )
          )
        )
        
      }
      
      
      movie_poster <- get_movie_poster(movie_info)
      fluidRow(
        h1(movie_title),
        div(id = "myImageBlank",
            tags$img(src = movie_poster, style = "width: 300px; height: 500px;")
        )   
      )
      
    }
    else if (user_backend$modelStatus == 'Base'){
      
      if(is.null(movie_being_displayed())){
        user_init_recs = unlist(user_backend$base_state_recs)
        movie_to_display = sample(user_init_recs, 1)
        movie_data_to_display_zero = movies %>% filter(id == movie_to_display)
        movie_for_upload(movie_data_to_display_zero)
      }
      else{
        movie_to_display = movie_being_displayed()
        movie_data_to_display_zero = movies %>% filter(id == movie_to_display)
        movie_for_upload(movie_data_to_display_zero)
        
      }
      
      
      
      
      
      movie_title = paste(movie_data_to_display_zero$title, "-", movie_data_to_display_zero$release_year)
      print(movie_title)
      # Get movie information
      movie_info <- get_movie_info(movie_data_to_display_zero$id)
      
      if (is.null(movie_info)) {
        return(
          fluidRow(
            h1(movie_title),
            div(id = "myImageBlank",
                tags$img(src = 'test_logo.png', style = "width: 300px; height: 500px;")
            )
          )
        )
        
      }
      
      
      movie_poster <- get_movie_poster(movie_info)
      fluidRow(
        h1(movie_title),
        div(id = "myImageBlank",
            tags$img(src = movie_poster, style = "width: 300px; height: 500px;")
        )   
      )
      
    }
    
    
    
    
  })
  
  # ...... MY MOVIES UI --------------------------------------------------------------------
  
  output$flickCounter <- renderValueBox_fixed({
    if(credentials()$user_auth == F){
      return()
    }
    user_to_display = credentials()$info$user
    user_backend = mongo_backing_reactive() %>% filter(entry_type == 'user_profile',username == user_to_display)
    if (nrow(user_backend) == 0) {
      return()
    }
    value_to_display = ifelse(is.null(user_backend$total_flicks), 0, user_backend$total_flicks)
    value_box_fixed(
      value_to_display, "TOTAL FLICKS", icon = icon("clapperboard"),
      color = "green"
    )
  })
  
  output$reviewCounter <- renderValueBox_fixed({
    if(credentials()$user_auth == F){
      return()
    }
    user_to_display = credentials()$info$user
    user_backend = mongo_backing_reactive() %>% filter(entry_type == 'user_profile',username == user_to_display)
    if (nrow(user_backend) == 0) {
      return()
    }
    value_to_display = ifelse(is.null(user_backend$total_flicks), 0, user_backend$total_flicks)
    
    value_box_fixed(
      value_to_display, "TOTAL REVIEWS", icon = icon("user-pen"),
      color = "aqua"
    )
  })
  
  output$genreCounter <- renderValueBox_fixed({
    if(credentials()$user_auth == F){
      return()
    }
    user_to_display = credentials()$info$user
    user_backend = mongo_backing_reactive() %>% filter(entry_type == 'user_profile',username == user_to_display)
    if (nrow(user_backend) == 0) {
      return()
    }
    user_genre_columns <- c(grep("^(GENRE_)", names(user_backend), value = TRUE))
    genre_columns = user_backend %>% select(user_genre_columns)
    transposed_genres = data.frame(t(genre_columns))
    names(transposed_genres) = 'count'
    transposed_genres = transposed_genres %>% arrange(desc(count)) %>% slice(1)
    genre_clean <- sub("^GENRE_", "", rownames(transposed_genres))  # Remove 'GENRE_'
    genre_clean <- sub("_", " ", genre_clean) # Replace underscores with spaces
    
    value_to_display = ifelse(is.null(genre_clean), 0, genre_clean)
    
    value_box_fixed(
      subtitle = 'TOP GENRES', value = HTML(genre_clean), icon = icon("ranking-star"),
      color = "teal"
    )
    
  })
  
  output$myMoviesTable = renderDT({
    if(credentials()$user_auth == F){
      return()
    }
    
    user_to_display = credentials()$info$user
    user_backend = mongo_backing_reactive() %>% filter(entry_type == 'user_profile',username == user_to_display)
    if (nrow(user_backend) == 0) {
      return()
    }
    ids_unlisted = unlist(user_backend$movies_right)
    
    movie_table_for_user = movies %>% filter(id %in% ids_unlisted) %>% select(
      title,
      release_year,   
      genres,
      tagline
      )
    
    names(movie_table_for_user) = c('TITLE', 'YEAR', 'GENRES', 'TAGLINE')
    datatable(
      movie_table_for_user,
      options = list(
        paging = TRUE,      # Enable pagination
        searching = TRUE,   # Enable search feature
        info = FALSE
        # Disable information display
      )
    )  })
  
  
  # ... SIDEBAR UI --------------------------------------------------------------------
  
  output$sidebarUI <- renderMenu({

    if(credentials()$user_auth == F){
      return()
    }
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("My Movies", tabName = "movies", icon = icon("film")),
      menuItem("My Ratings", tabName = "ratings", icon = icon("star")),
      menuItem("Settings", tabName = "settings", icon = icon("gear"))
    )
  })
  
  observeEvent(c(sidebar_initialized()),{
    updateTabItems(session, "tabs", "home")
  })

  
  
}

shinyApp(ui = ui, server = server)
