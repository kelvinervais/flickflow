
# Import Packages ---------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinysense)
library(ggplot2)


# Create Variables --------------------------------------------------------

genre_col <- c("ROMANCE","DRAMA","HORROR","TVMOVIE",
            "CRIME","THRILLER","ACTION",
            "ADVENTURE","MYSTERY","SCIENCEFICTION",
            "ANIMATION","DOCUMENTARY","MUSIC",
            "FAMILY","WESTERN","FANTASY",
            "HISTORY","WAR")

hours_col <- c(22.0, 30.0, 0.0, 100.0,
               55.0, 66.0, 0.0, 1.0,
               32.5, 74.0, 80.0, 5.0,
               77.0, 23.0, 120.0, 65.0,
               80.5, 26.2)

bar_df = data.frame(genre_col,hours_col)

generation_col <- c("1940's", "1950's","1960's",
                    "1970's","1980's","1990's",
                    "2000's","2010's","2020's")


# Load and Clean Movie Data -----------------------------------------------

### Can use below three lines to load and clean from csv, or use third line to directly load data

#movie_data_url_all <- read.csv('movie_data_url.csv')
#movie_data_url_clean <-  movie_data_url_all[, colnames(movie_data_url_all)[c(3:28,107)]]
#save(movie_data_url_clean,file="movie_data_url_clean.RData")

load("movie_data_url_clean.RData")

# Create UI ---------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "FlickFlow"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Zero State Model", tabName = "zeromodel", icon = icon("th")),
      menuItem("Base State Model", icon = icon("dashboard"), tabName = "basemodel",
               badgeLabel = "new", badgeColor = "green"),
      menuItem("Recommended Movies", icon = icon("dashboard"), tabName = "recommend",
               badgeLabel = "new", badgeColor = "red"),
      menuItem("User FilmTaste Analytics", icon = icon("dashboard"), tabName = "analytics"
               )
      )
    ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "basemodel",
              fluidPage(
                h1("Base State Model"),
                p("Welcome to FlickFlow, a people-driven movie recommendation engine!"),
                p("Swipe Left if you Dislike, Swipe Right if you Like!"),
                hr(),
                shinyswipr_UI( "swiper_base",
                               fluidRow(
                                 column(3, align = "center",
                                        h4("Movie Title")
                                        ),
                                 column(6, align = "center",
                                        textOutput("title_base")
                                        )
                                 ),
                               hr(),
                               fluidRow(
                                 column(3, align = "center",
                                        h4("Movie Poster")
                                        ),
                                 column(6, align = "center",
                                        imageOutput("poster_base", width = 280, height = 400)
                                        )
                                 ),
                               hr(),
                               fluidRow(
                                 column(3, align = "center",
                                        h4("Plot Overview")
                                        ),
                                 column(6, align = "center",
                                        textOutput("plot_base")
                                        )
                                 )
                               ),
                hr(),
                h4("Swipe History"),
                tableOutput("resultsTable_base")
                )
              ),
      tabItem(tabName = "zeromodel",
              fluidPage(
                h1("Zero State Model"),
                fluidRow(
                  box(
                    title = "Preferred Genre Selection",
                    width = 3,
                    background = "light-blue",
                    checkboxGroupInput("genre", "What film genres do you like?", genre_col)
                         ),
                  box(
                    title = "What is your ideal movie runtime?",
                    background = "green",
                    sliderInput("runtime","Preferred Movie Runtime",30,180,120,step = 10)
                    ),
                  box(
                    title = "Preferred Era of Film",
                    width = 3,
                    background = "orange",
                    radioButtons("generation", "What is your favorite film generation?", generation_col)
                    )
                  )
                )
              ),
      tabItem(tabName = "recommend",
              fluidPage(
                h1("Your Recommended Movies"),
                p("Based off your Base Model results, we recommend you try these movies!"),
                p("If this movie is new to you, Swipe Left if you Dislike, Swipe Right if you Like!"),
                p("If you've seen this movie before, Swipe Down if you Dislike, Swipe Up if you Like!"),
                hr(),
                shinyswipr_UI( "swiper_rec",
                               fluidRow(
                                 column(3, align = "center",
                                        h4("Movie Title")
                                 ),
                                 column(6, align = "center",
                                        textOutput("title_rec")
                                 )
                               ),
                               hr(),
                               fluidRow(
                                 column(3, align = "center",
                                        h4("Movie Poster")
                                 ),
                                 column(6, align = "center",
                                        imageOutput("poster_rec", width = 280, height = 400)
                                 )
                               ),
                               hr(),
                               fluidRow(
                                 column(3, align = "center",
                                        h4("Plot Overview")
                                 ),
                                 column(6, align = "center",
                                        textOutput("plot_rec")
                                 )
                               )
                ),
                hr(),
                h4("Swipe History"),
                tableOutput("resultsTable_rec")
              )
      ),
      tabItem(tabName = "analytics",
              fluidPage(
                h1("Your Movie Preference Analytics"),
                h2("Hour vs Genre Distrubution of Movies Watched in 2024"),
                plotOutput("barplot")
                )
              )
      )
    )
  )



# Create Server -----------------------------------------------------------

server <- function(input, output, session) {
  card_swipe_base <- callModule(shinyswipr, "swiper_base")
  card_swipe_rec <- callModule(shinyswipr, "swiper_rec")
  load("movie_data_url_clean.RData")
  
  output$poster_base <- renderImage({
    filename <- normalizePath(file.path('./www/images','fightclubsmall.jpg'))

    list(src = filename,
         width = 280,
         height = 400,
         contentType = 'image/png',
         alt = "This is alt text")
  }, deleteFile = FALSE)
  
  output$poster_rec <- renderImage({
    filename <- normalizePath(file.path('./www/images','avp.jpg'))

    list(src = filename,
         width = 280,
         height = 400,
         contentType = 'image/png',
         alt = "This is alt text")
  }, deleteFile = FALSE)

  output$barplot <- renderPlot({
    
    ggplot(data=bar_df,aes(x=genre_col,y=hours_col)) +
    geom_bar(stat="identity",fill="steelblue") +
    geom_text(aes(label=hours_col), vjust=-0.3, size=3.5)
  })
  
  observeEvent( card_swipe_base(),{
    print(card_swipe_base) #show last swipe result. 
  })
  
  observeEvent( card_swipe_base(),{
    print(card_swipe_rec) #show last swipe result. 
  })
}
  
##### Below is the code written to be reactive and automatically update the movie card after a swipe
##### I'm still troubleshooting it, so it's commented out for now
  
  # swipe_count_nr = 1
  # load("movie_data_url_clean.RData")
  # card_swipe <- callModule(shinyswipr, "movie_swiper")
  # 
  # swipe_count = reactive({
  #   invalidateLater(1000,session)
  #   return(swipe_count_nr)
  # })
  # 
  # appVals <- reactiveValues(
  #   movie = movie_data_url_clean[swipe_count(),],
  #   swipes = data.frame(mtitle = character(), 
  #                       poster = character(),
  #                       overview = character(),
  #                       swipe = character())
  # )
  # 
  # our_movie <- isolate(appVals$movie)
  # 
  # output$mtitle <- renderText({ our_movie$movie_title })
  # output$poster <- renderText({ our_movie$poster_url })
  # output$overview <- renderText({ our_movie$overview })
  # 
  # output$resultsTable <- renderDataTable({appVals$swipes})
  # 
  # observeEvent( card_swipe(),{
  #   #Record our last swipe results.
  #   appVals$swipes <- rbind(
  #     data.frame(mtitle = appVals$movie$movie_title,
  #                poster = appVals$movie$poster_url,
  #                overview = appVals$movie$overview,
  #                swipe = card_swipe()
  #     ), appVals$swipes
  #   )
  #   #send results to the output.
  #   output$resultsTable <- renderTable({appVals$swipes})
  # 
  #   #update the movie
  #   swipe_count_nr<<-swipe_count_nr+1
  #   appVals$movie <- movie_data_url_clean[swipe_count(),]
  #   
  #   # updateReactable(
  #   #   appVals$movie,
  #   #   data = movie_data_url_clean[i,]
  #   # )
  #   
  #   #send update to the ui.
  #   output$mtitle <- renderText({ appVals$our_movie$movie_title })
  #   output$poster <- renderText({ appVals$our_movie$poster_url })
  #   output$overview <- renderText({ appVals$our_movie$overview })
  # }) #close event observe.
#}

  
  
  


# Run Application ---------------------------------------------------------

shinyApp(ui, server)
