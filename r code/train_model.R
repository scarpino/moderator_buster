#SVScarpino

#libraries
library(keras)

#global parameters
data_set <- "equalize_17" #switch to 10k17 to get the earlier, 10k data set
max_features <- 25000
epochs <- 10
batch_size <- 500
layer_drop <- 0.2

#acc functions
remove_punc <- function(x, reg_ex = '[[:punct:]]'){
  x_out <- gsub(reg_ex,' ',x)
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

get_max <- function(x){
  return(which(x == max(x, na.rm = TRUE)))
}

#data
data_set <- "equalize_17" #switch to 10k17 to get the earlier, 10k data set
if(data_set == "10k17"){
  load("../data/1518093618.77705arxiv_2017_10k.RData")
  arxiv_raw <- arxiv_2017_10k
}else{
  if(data_set == "equalize_17"){
    load("../data/1518380587.94836_arxiv_2017_equalize.RData")
    arxiv_raw <- data.frame(NA, NA)
    colnames(arxiv_raw) <- c("primary_categories_i", "summaries_i")
    for(i in 1:length(arxiv_2017_equalize)){
      arxiv_raw <- rbind(arxiv_raw, arxiv_2017_equalize[[i]][,c("primary_categories_i", "summaries_i")])
    }
    colnames(arxiv_raw) <- c("primary_categories", "summaries") #I know this is lazy
  }else{
    stop("Data set not supported")
  }
}


#let's do some lazy pre-processing
#0. Remove NA categories
rm_na <- which(is.na(arxiv_raw$primary_categories) == TRUE)
if(length(rm_na) > 0){
  arxiv_raw <- arxiv_raw[-rm_na,]
}

#1. remove punctuation
summaries_no_punc <- rep(NA, length(arxiv_raw$summaries))
for(i in 1:nrow(arxiv_raw)){
  no_new_line.i <- remove_punc(as.character(arxiv_raw$summaries[i]), reg_ex = "\n") #note as.character because of forgetting factors earlier
  no_new_line_no_punc.i <- remove_punc(no_new_line.i, reg_ex = '[[:punct:]]')
  summaries_no_punc[i] <- no_new_line_no_punc.i
}

#2. filter max_features most common words
corpus_words_list <- lapply(summaries_no_punc, extract_words)
corpus_words <- unlist(corpus_words_list)
word_counts <- table(corpus_words)
if(length(word_counts) > max_features){
  allowed_words <- names(word_counts[order(word_counts, decreasing = TRUE)])[20:(max_features+19)]
}else{
  allowed_words <- names(word_counts)
}

filtered_summaries <- rep(NA, length(summaries_no_punc))
for(i in 1:length(summaries_no_punc)){
  filtered_summaries[i] <- filter_words(summaries_no_punc[i], allowed_words = allowed_words, replace_word = "STRONGCAT")
}
 
#3. truncate/pad to 150 words
trunc_fill_words <- rep(NA, length(filtered_summaries))
for(i in 1:length(filtered_summaries)){
  trunc_fill_words[i] <- trunc_pad(filtered_summaries[i], n_words = 150, filler_word = "STRONGCAT")
}

#4. Create y categories
y_raw <- as.character(arxiv_raw$primary_categories)
y_use_list <- lapply(X = y_raw, FUN = make_categories)
y_use <- unlist(y_use_list) 
possible_categories <- unique(y_use)

#5. split into training/testing and create a matrix with the words
use_train <- sample(1:length(trunc_fill_words), length(trunc_fill_words)*0.8)

x_train <- vectorize_sequences(sequences = trunc_fill_words[use_train], allowed_words = allowed_words, filler_word = "STRONGCAT") #this throws away a lot of information and won't allow for convolutions. Should fix later
y_train <- vectorize_sequences(sequences = y_use[use_train], allowed_words = possible_categories, filler_word = NULL)

x_test <- vectorize_sequences(sequences = trunc_fill_words[-use_train], allowed_words = allowed_words, filler_word = "STRONGCAT") 
y_test <- vectorize_sequences(sequences = y_use[-use_train], allowed_words = possible_categories, filler_word = NULL)

#here we go
model <- keras_model_sequential() %>% 
  layer_dense(units = length(possible_categories)*5, activation = "relu", input_shape = (max_features+1)) %>% 
  layer_dropout(layer_drop) %>%
  layer_dense(units = length(possible_categories)*4, activation = "relu") %>% 
  layer_dropout(layer_drop) %>%
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
barplot(rich_prop_cor[order(rich_prop_cor, decreasing = TRUE)], names = possible_categories[order(rich_prop_cor, decreasing = TRUE)], ylim = c(0,1), ylab = "OOS Accuracy", main = "OOS Accuracy by category", las = 2, cex.names = 0.8)
results

#saving model
save_model_hdf5(model, filepath = "../models/arxiv_raw", overwrite = TRUE, include_optimizer = TRUE)

#saving files for r shiny
save_model_hdf5(model, filepath = "../moderator_buster/arxiv_raw", overwrite = TRUE, include_optimizer = TRUE)

save(x_test, file = "../moderator_buster/x_test.RData")
save(y_test, file = "../moderator_buster/y_test.RData")
save(results_oos, file = "../moderator_buster/results_OOS.RData")
save(rich_prop_cor, file = "../moderator_buster/rich_prop_cor.RData")

possible_categories <- unique(y_use)
save(possible_categories, file = "../moderator_buster/possible_categories.RData")
save(allowed_words, file = "../moderator_buster/allowed_words.RData")