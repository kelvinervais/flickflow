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

admin_accounts = c('kervais')

if (app_type == 'dev'){
  load("~/Desktop/flickflow/UI/flickflow/flickflow_base_workspace.RData")
  
}
if (app_type == 'prod'){
  load("flickflow_base_workspace.RData")
}
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
# UI --------------------------------------------------------------------

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
       shinyswipr_UI( "zeroModelSwipr", 
                      fluidRow(
                        column(12,
                               align = 'center',
                               uiOutput('homeRecDisplay') 
                               )
  
            
              
       )
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
    
    observeEvent(input$signupConfirmation, {
      
      signup_usename = input$signupUsername
      signup_password= input$signupPassword
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
      
      initial_pick = strsplit(input$signupMoviePicks, split = '-')
      
      initial_recommendation = movies %>% filter(release_year == initial_pick[[1]][2], grepl(initial_pick[[1]][1], title))
      
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
    
    card_swipe_zero<- callModule(shinyswipr, "zeroModelSwipr")
    
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
      user_backend_upsert_columns$movies_flicked =  list(append(unlist(user_backend$movies_flicked),movie_swiped$id))
      
      user_backend_upsert_columns$popularity_percentile = mean(movies_swiped_so_far$popularity)
      user_backend_upsert_columns$vote_average_percentile = mean(movies_swiped_so_far$vote_average)
      user_backend_upsert_columns$revenue_percentile = mean(movies_swiped_so_far$revenue)
      user_backend_upsert_columns$budget_percentile = mean(movies_swiped_so_far$budget)
      

      user_init_recs = unlist(user_backend$zero_state_recs)
      user_init_recs <- user_init_recs[user_init_recs != movie_swiped$id ]
      user_backend_upsert_columns$zero_state_recs = list(user_init_recs)
      


      if (user_backend$modelStatus == 'Zero'){
        movie_to_display = sample(user_init_recs, 1)
        movie_being_displayed(movie_to_display)
      }
      
      upsert_data(user_backend_upsert_columns, user_backend$username)
      
      mongo_backing_reactive(mongo_database$find())
      
    })

    

    
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
    
    if((input$signupUsername %in% user_base_reactive()$user == T)){
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
    movie_titles <- gsub("-", "", recommended_movies$title)
    
    movies_plus_year = paste0(movie_titles,'-', recommended_movies$release_year)
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
      tabItem(tabName = "movies", h2("My Movies")),
      tabItem(tabName = "ratings", h2("My Ratings")),
      tabItem(tabName = "settings", h2("Settings"))
    )    
  })
  
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
    
    
    
    
  })
  
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
