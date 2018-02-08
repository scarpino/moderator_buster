#SVScarpino

#notes
#this was super helpful for R syntax, which isn't well documented yet https://tensorflow.rstudio.com/keras/articles/examples/imdb_cnn.html

#libraries
library(keras)

#global params
text_length <- 150 #this is the total number of words required in the abstract (will trunc/fill to this number)
max_features <- 15000
batch_size <- 100
embedding_dims <- 50
filters <- 250
kernel_size <- 3
epochs <- 20
layer_drop <- 0.2

#acc functions
remove_punc <- function(x, reg_ex = '[[:punct:]]'){
  x_out <- gsub(reg_ex,'',x)
  return(x_out)
}

extract_words <- function(x){
  x_split <- strsplit(x = x, " ")  
  x_split_unlist <- unlist(x_split)
  x_out <- x_split_unlist[which(x_split_unlist != "")]
  return(x_out)
}

filter_words <- function(x, allowed_words, replace_word){
  words <- extract_words(x)
  rm <- which(! words %in% allowed_words)
  if(length(rm) > 0){
    words[rm] <- replace_word
  }
  words_out <- paste(words, collapse = " ")
  return(words_out)
}

trunc_pad <- function(x, n_words, filler_word){
  words <- extract_words(x)
  if(length(words) == n_words){
    words_out <- paste(words, collapse = " ")
  }else{
    if(length(words) > n_words){
      words_out <- paste(words[1:n_words], collapse = " ")
    }else{
      diff_len <- n_words - length(words)
      words_out <- paste(c(words, rep(filler_word, diff_len)), collapse = " ")
    }
  }
  return(words_out)
}

make_categories <- function(x){
  x_split <- strsplit(x = x, "[.]")  
  x_split_unlist <- unlist(x_split)[1]
  return(x_split_unlist)
}

vectorize_sequences <- function(sequences, allowed_words, filler_word) {
  #I shamelessly stole some of this from https://tensorflow.rstudio.com/blog/text-classification-with-keras.html
  # Creates an all-zero matrix of shape (length(sequences), dimension)
  allowed_words <- c(allowed_words, filler_word)
  results <- matrix(0, nrow = length(sequences), ncol = length(allowed_words))
  for (i in 1:length(sequences)){
    words_i <- extract_words(sequences[i])
    mt_i <- match(words_i, allowed_words)
    results[i, mt_i] <- 1 
  }
  return(results)
}

positionize_sequences <- function(sequences, allowed_words, filler_word, text_length) {
  #I shamelessly stole some of this from https://tensorflow.rstudio.com/blog/text-classification-with-keras.html
  # Creates an all-zero matrix of shape (length(sequences), dimension)
  allowed_words <- c(allowed_words, filler_word)
  results <- matrix(0, nrow = length(sequences), ncol = text_length)
  for (i in 1:length(sequences)){
    words_i <- extract_words(sequences[i])
    mt_i <- match(words_i, allowed_words)
    results[i, ] <- mt_i
  }
  return(results)
}

get_max <- function(x){
  return(which(x == max(x, na.rm = TRUE)))
}

#data
load("../data/1518093618.77705arxiv_2017_10k.RData")

#let's do some lazy pre-processing
#1. remove punctuation
summaries_no_punc <- rep(NA, length(arxiv_2017_10k$summaries))
for(i in 1:nrow(arxiv_2017_10k)){
  no_new_line.i <- remove_punc(as.character(arxiv_2017_10k$summaries[i]), reg_ex = "\n") #note as.character because of forgetting factors earlier
  no_new_line_no_punc.i <- remove_punc(no_new_line.i, reg_ex = '[[:punct:]]')
  summaries_no_punc[i] <- no_new_line_no_punc.i
}

#2. filter on 5k most common and 10k least common words
corpus_words_list <- lapply(summaries_no_punc, extract_words)
corpus_words <- unlist(corpus_words_list)
word_counts <- table(corpus_words)
if(length(word_counts) > max_features){
  allowed_words <- c(names(word_counts[order(word_counts, decreasing = TRUE)])[1:(max_features*(1/3))], names(word_counts[order(word_counts, decreasing = FALSE)])[1:(max_features*(2/3))]) #sorry for this syntax
}else{
  allowed_words <- names(word_counts)
}

filtered_summaries <- rep(NA, length(summaries_no_punc))
for(i in 1:length(summaries_no_punc)){
  filtered_summaries[i] <- filter_words(summaries_no_punc[i], allowed_words = allowed_words, replace_word = "STRONGCAT")
}
 
#3. truncate/pad to text_length words
trunc_fill_words <- rep(NA, length(filtered_summaries))
for(i in 1:length(filtered_summaries)){
  trunc_fill_words[i] <- trunc_pad(filtered_summaries[i], n_words = text_length, filler_word = "STRONGCAT")
}

#4. Create y categories
y_raw <- as.character(arxiv_2017_10k$primary_categories)
y_use_list <- lapply(X = y_raw, FUN = make_categories)
y_use <- unlist(y_use_list) 
possible_categories <- unique(y_use)

#5. split into training/testing and create a matrix with the words
use_train <- sample(1:length(trunc_fill_words), length(trunc_fill_words)*0.8)

x_train <- positionize_sequences(sequences = trunc_fill_words[use_train], allowed_words = allowed_words, filler_word = "STRONGCAT", text_length = text_length) #this throws away a lot of information and won't allow for convolutions. Should fix later
y_train <- vectorize_sequences(sequences = y_use[use_train], allowed_words = possible_categories, filler_word = NULL)

x_test <- positionize_sequences(sequences = trunc_fill_words[-use_train], allowed_words = allowed_words, filler_word = "STRONGCAT", text_length = text_length) 
y_test <- vectorize_sequences(sequences = y_use[-use_train], allowed_words = possible_categories, filler_word = NULL)

#here we go
model <- keras_model_sequential() %>% 
  layer_embedding(max_features+2, embedding_dims, input_length = text_length) %>% 
  layer_dropout(layer_drop) %>%
  layer_conv_1d(filters, kernel_size, padding = "valid", activation = "relu", strides = 1) %>%
  layer_global_max_pooling_1d() %>%
  layer_dense(units = length(possible_categories), activation = "softmax")

model %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)


history <- model %>% fit(
  x_train,
  y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = list(x_test, y_test)
)

#validating on held out data
results <- model %>% evaluate(x_test, y_test)

#example for prediction
model %>% predict(x_test[1:2,])
y_test[1:2,]

#eval oos
results_oos <- model %>% evaluate(x_test, y_test)
pred_oos <- model %>% predict(x_test)

true_val <- apply(y_test, 1, get_max)
pred_val <- apply(pred_oos, 1, get_max)
pred_conf <- table(true_val, pred_val)
#prop_cor <- diag(pred_conf)/colSums(y_test)
rich_cor <- t(pred_oos) %*% y_test
rich_prop_cor <- diag(rich_cor)/colSums(y_test) 

#saving model
save_model_hdf5(model, filepath = "../models/arxiv_2017_10k", overwrite = TRUE, include_optimizer = TRUE)

save(x_test, file = "../moderator_buster/x_test.RData")
save(y_test, file = "../moderator_buster/y_test.RData")
save(results_oos, file = "../moderator_buster/results_OOS.RData")
save(rich_prop_cor, file = "../moderator_buster/rich_prop_cor.RData")

save(possible_categories, file = "../moderator_buster/possible_categories.RData")
save(allowed_words, file = "../moderator_buster/allowed_words.RData")
