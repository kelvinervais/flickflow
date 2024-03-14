# Load necessary libraries
library(tm)
library(caret)
library(e1071)
library(ROCR)
library(naivebayes)
library(randomForest)

# Load IMDb reviews dataset (replace 'imdb_reviews.csv' with your actual dataset file)
data <- read.csv('/Users/kelvinervais/Desktop/flickflow/Uncompressed Data/IMDB Dataset.csv', stringsAsFactors = FALSE)

# Train-test split
set.seed(123)
train_index <- createDataPartition(data$sentiment, p = 0.7, list = FALSE)
train <- data[train_index, ]
test <- data[-train_index, ]

# Data preprocessing
corpus_train <- Corpus(VectorSource(train$review))
corpus_train <- tm_map(corpus_train, tolower)
corpus_train <- tm_map(corpus_train, removePunctuation)
corpus_train <- tm_map(corpus_train, removeNumbers)
corpus_train <- tm_map(corpus_train, removeWords, stopwords("en"))
corpus_train <- tm_map(corpus_train, stripWhitespace)

corpus_test <- Corpus(VectorSource(test$review))
corpus_test <- tm_map(corpus_test, tolower)
corpus_test <- tm_map(corpus_test, removePunctuation)
corpus_test <- tm_map(corpus_test, removeNumbers)
corpus_test <- tm_map(corpus_test, removeWords, stopwords("en"))
corpus_test <- tm_map(corpus_test, stripWhitespace)

# Create document-term matrix for both train and test sets
dtm_train <- DocumentTermMatrix(corpus_train)
dtm_train <- removeSparseTerms(dtm_train, 0.1)

dtm_test <- DocumentTermMatrix(corpus_test, control = list(dictionary = Terms(dtm_train)))
dtm_test <- removeSparseTerms(dtm_test, 0.1)

# Convert to data frame
train_reviews <- as.data.frame(as.matrix(dtm_train))
colnames(train_reviews) <- make.names(colnames(train_reviews))
train_reviews$sentiment <- train$sentiment
train_review_list = c()
for (corpus_review in 1:length(corpus_train)){
  print(corpus_review)
  
  train_review_list = c(train_review_list, corpus_train[[corpus_review]][[1]])
}
train_reviews$review = train_review_list


test_reviews <- as.data.frame(as.matrix(dtm_test))
colnames(test_reviews) <- make.names(colnames(test_reviews))
test_reviews$sentiment <- test$sentiment
test_review_list = c()

for (corpus_review in 1:length(corpus_test)){
  print(corpus_review)
  
  test_review_list = c(test_review_list, corpus_test[[corpus_review]][[1]])
}
test_reviews$review = test_review_list
# Build the Naive Bayes model
model <- naive_bayes(as.factor(sentiment) ~ ., data = train_reviews)

# Make predictions
predictions <- predict(model, newdata = data.frame(test_reviews$review))

# Calculate accuracy
accuracy <- mean(predictions == test_reviews$sentiment)
cat("Accuracy:", accuracy, "\n")

# Calculate probabilities
probabilities <- predict(model, newdata = test_reviews, type = "prob")
head(probabilities)


# Build the model (Random Forest)
model <- randomForest(as.factor(sentiment) ~ ., data = train_reviews)

# Make predictions
predictions <- predict(model, newdata = test_reviews)

# Calculate accuracy
accuracy <- mean(predictions == test_reviews$sentiment)
cat("Accuracy:", accuracy, "\n")

