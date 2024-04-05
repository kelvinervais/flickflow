# IMPORTS ---------------------------------------------------------------

library(dplyr)
library(mongolite)
library(jsonlite)

movies = read.csv('/Users/kelvinervais/Desktop/flickflow/UI/flickflow-workspace/movie_data.csv')
movies = movies %>% distinct(id, .keep_all = TRUE)




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
  
  
  mongo_database$update(paste0('{"entry_type": "user_response",
                           "User": "',user,'"}'),
                        paste0('{"$set":', toJSON_updater(upsert_content), '}'),
                        upsert = TRUE)
  
}
#mongo_database$drop()
data <- mongo_database$find('{}')

# MONGO INIT ---------------------------------------------------------------
# guid = 'testing'
# signup_usename = 'testing'
# signup_password = 'testing'
# date_added = '2024-04-02'
# mongo_df = data.frame(
#   entry_type = 'user_profile', #Can be types User Profile, User Entries
#   guidUserId =  guid, #Global identifier for users, will be generated upon account creation and for any interaction
#   username = signup_usename, #username for account, only under type User Profile
#   password = signup_password, #password for account, only under type User Profile
#   date_joined = date_added, #date account created, only under type User Profile
#   flicks_up = 0, #Running number of flicks up, only under type User Profile
#   flicks_down = 0 , #Running number of flicks down, only under type User Profile
#   flicks_right = 0, #Running number of flicks right, only under type User Profile
#   flicks_left = 0, #Running number of flicks left, only under type User Profile,
#   movies_flicked = c('Init'), #Running list of movie ids that were flicked, only under type User Profile
#   #User My Movies Columns
#   movies_up = c('Init'), #Running list of movie ids that were flicked up, only under type User Profile
#   movies_down = c('Init'), #Running list of movie ids that were flicked down, only under type User Profile
#   movies_right = c('Init'), #Running list of movie ids that were flicked right, only under type User Profile
#   movies_left = c('Init') #Running list of movie ids that were flicked left, only under type User Profile
# )





save(movies, file = "~/Desktop/flickflow/UI/flickflow/flickflow_base_workspace.RData")
