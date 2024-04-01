library(tm)
library(SnowballC)
library(caret)

# Load IMDb reviews dataset
data <- read.csv('/Users/kelvinervais/Desktop/flickflow/Uncompressed Data/IMDB Dataset.csv', stringsAsFactors = FALSE)

# Preprocessing
corpus <- Corpus(VectorSource(data$review))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stemDocument)

# Create document term matrix
dtm <- DocumentTermMatrix(corpus)
dtm_sparse <- removeSparseTerms(dtm, 0.95)

# Convert to data frame
dtm_df <- as.data.frame(as.matrix(dtm_sparse))
colnames(dtm_df) <- make.names(colnames(dtm_df))

# Add sentiment labels
dtm_df$sentiment <- ifelse(data$sentiment == 'positive', 1, 0)

# Split dataset into training and testing sets
set.seed(123)
train_index <- createDataPartition(dtm_df$sentiment, p = 0.8, list = FALSE)
train_data <- dtm_df[train_index, ]
test_data <- dtm_df[-train_index, ]

# Train Logistic Regression model
model <- glm(sentiment ~ ., data = train_data, family = binomial)

# Predictions
predicted_classes_prob <- predict(model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predicted_classes_prob > 0.5, 1, 0)

test_data$sentiment <- factor(test_data$sentiment, levels = c(0, 1))

# Convert predicted_classes to factor with levels
predicted_classes <- factor(predicted_classes, levels = levels(test_data$sentiment))

# Evaluate model
confusionMatrix(predicted_classes, test_data$sentiment)

