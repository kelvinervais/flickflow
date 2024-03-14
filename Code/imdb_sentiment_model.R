
library(dplyr)
library(caret)
library(tm)

imdb_data = read.csv('/Users/kelvinervais/Desktop/flickflow/Uncompressed Data/IMDB Dataset.csv')




# Create a corpus
corpus <- Corpus(VectorSource(imdb_data$review))

# Text preprocessing
corpus <- tm_map(corpus, content_transformer(tolower)) # Convert to lowercase
corpus <- tm_map(corpus, removePunctuation) # Remove punctuation
corpus <- tm_map(corpus, removeNumbers) # Remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("en")) # Remove stopwords
corpus <- tm_map(corpus, stripWhitespace) # Strip whitespace

# Create a document-term matrix
dtm <- DocumentTermMatrix(corpus)

# Split data into training and testing sets
set.seed(123) # for reproducibility
trainIndex <- createDataPartition(imdb_data$sentiment, p = .7, 
                                  list = FALSE, 
                                  times = 1)
train_data <- imdb_data[trainIndex, ]
test_data <- imdb_data[-trainIndex, ]

# Train the sentiment analysis model
model <- train(as.factor(sentiment) ~ ., 
               data = train_data, 
               method = "glm", # Generalized Linear Model
               trControl = trainControl(method = "cv", number = 5), 
               preProcess = c("scale", "center")) 

# Make predictions on the test set
predictions <- predict(model, newdata = test_data)

# Evaluate the model
confusionMatrix(predictions, test_data$sentiment)