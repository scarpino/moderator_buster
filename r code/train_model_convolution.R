#SVScarpino

#notes
#this was super helpful for R syntax, which isn't well documented yet https://tensorflow.rstudio.com/keras/articles/examples/imdb_cnn.html

#libraries
library(keras)

#global params
data_set <- "equalize_17" #switch to 10k17 to get the earlier, 10k data set
make_equal <- FALSE #sample with replacement to get equal numbers of categories
text_length <- 150 #this is the total number of words required in the abstract (will trunc/fill to this number)
max_features <- 25000
batch_size <- 200
embedding_dims <- 250
hidden_dims <- 150
filters <- 500
kernel_size <- 3
epochs <- 12
layer_drop <- 0.3

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
data_set <- "equalize_17" #switch to 10k17 to get the earlier, 10k data set
if(data_set == "10k17"){
  load("../data/1518093618.77705arxiv_2017_10k.RData")
  arxiv_raw <- arxiv_raw
}else{
  if(data_set == "equalize_17"){
    load("../data/1518380587.94836_arxiv_2017_equalize.RData")
    arxiv_raw <- data.frame(NA, NA)
    colnames(arxiv_raw) <- c("primary_categories_i", "summaries_i")
    for(i in 1:length(arxiv_2017_equalize)){
      summaries_i <- as.character(arxiv_2017_equalize[[i]]$summaries_i)
      #primary_categories_i <- as.character(arxiv_2017_equalize[[i]]$primary_categories_i) #these seem to be inaccurate
      primary_categories_raw_i <- as.character(arxiv_2017_equalize[[i]]$full_categories_i)
      primary_categories_i <- rep(NA, length(primary_categories_raw_i))
      for(j in 1:length(primary_categories_raw_i)){
        primary_categories_i.j <- strsplit(primary_categories_raw_i[j], "[ ]")[[1]][1]
        primary_categories_i.j.split <-  strsplit(primary_categories_i.j, "[.]")[[1]][1]
        primary_categories_i[j] <- primary_categories_i.j.split
      }
      arxiv_raw <- rbind(arxiv_raw, data.frame(summaries_i,primary_categories_i))
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

#2. filter on max_features most common words
corpus_words_list <- lapply(summaries_no_punc, extract_words)
corpus_words <- unlist(corpus_words_list)
word_counts <- table(corpus_words)
if(length(word_counts) > max_features){
  allowed_words <- names(word_counts[order(word_counts, decreasing = TRUE)])[10:(max_features+9)]
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
y_raw <- as.character(arxiv_raw$primary_categories)
y_use_list <- lapply(X = y_raw, FUN = make_categories)
y_use <- unlist(y_use_list) 
possible_categories <- unique(y_use)

#5. Make equal categories
if(make_equal == TRUE){
  equal_samp <- round(length(y_use)/length(possible_categories))
  use_equalize <- c()
  for(i in possible_categories){
    pick_i <- sample(which(y_use == i), equal_samp, replace = TRUE)
    use_equalize <- c(use_equalize, pick_i)
  }
  eval_oos_equalize <- which(! (1:length(y_use)) %in% use_equalize)
  trunc_fill_words_oos_equalize <- positionize_sequences(sequences = trunc_fill_words[eval_oos_equalize], allowed_words = allowed_words, filler_word = "STRONGCAT", text_length = text_length) 
  y_use_oos_equalize <- vectorize_sequences(sequences = y_use[eval_oos_equalize], allowed_words = possible_categories, filler_word = NULL)
  trunc_fill_words <- trunc_fill_words[use_equalize]
  y_use <- y_use[use_equalize]
  
}

#5. split into training/testing and create a matrix with the words
use_train <- sample(1:length(trunc_fill_words), length(trunc_fill_words)*0.8)

x_train <- positionize_sequences(sequences = trunc_fill_words[use_train], allowed_words = allowed_words, filler_word = "STRONGCAT", text_length = text_length) 
y_train <- vectorize_sequences(sequences = y_use[use_train], allowed_words = possible_categories, filler_word = NULL)

x_test <- positionize_sequences(sequences = trunc_fill_words[-use_train], allowed_words = allowed_words, filler_word = "STRONGCAT", text_length = text_length) 
y_test <- vectorize_sequences(sequences = y_use[-use_train], allowed_words = possible_categories, filler_word = NULL)

#here we go
model <- keras_model_sequential() %>% 
  layer_embedding(max_features+2, embedding_dims, input_length = text_length) %>% 
  layer_dropout(layer_drop) %>%
  layer_conv_1d(filters, kernel_size, padding = "valid", activation = "relu", strides = 1) %>%
  layer_global_max_pooling_1d() %>%
  layer_dense(units = length(possible_categories)*5, activation = "relu") %>%
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

results

if(make_equal == TRUE){
  results_oos_equalize <- model %>% evaluate(trunc_fill_words_oos_equalize, y_use_oos_equalize)
  results_oos_equalize
  
  pred_oos_equalize <- model %>% predict(trunc_fill_words_oos_equalize)
  rich_cor_oos_equalize <- t(pred_oos_equalize) %*% y_use_oos_equalize
  rich_prop_cor_oos_equalize <- diag(rich_cor_oos_equalize)/colSums(y_use_oos_equalize) 
  barplot(rich_prop_cor_oos_equalize[order(rich_prop_cor_oos_equalize, decreasing = TRUE)], names = possible_categories[order(rich_prop_cor_oos_equalize, decreasing = TRUE)], ylim = c(0,1), ylab = "OOS Accuracy", main = "OOS Accuracy by category", las = 2, cex.names = 0.8)
}
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
cat_dist_test <- colSums(y_test)
barplot(rich_prop_cor[order(rich_prop_cor, decreasing = TRUE)], names = possible_categories[order(rich_prop_cor, decreasing = TRUE)], ylim = c(0,1), ylab = "OOS Accuracy", main = "OOS Accuracy by category", las = 2, cex.names = 0.8)

#saving model
save_model_hdf5(model, filepath = "../models/arxiv_raw_convo", overwrite = TRUE, include_optimizer = TRUE)

#saving files for r shiny
save_model_hdf5(model, filepath = "../moderator_buster/arxiv_raw_convo", overwrite = TRUE, include_optimizer = TRUE)

save(x_test, file = "../moderator_buster/x_test_convo.RData")
save(y_test, file = "../moderator_buster/y_test_convo.RData")
save(results_oos, file = "../moderator_buster/results_OOS_convo.RData")
save(rich_prop_cor, file = "../moderator_buster/rich_prop_cor_convo.RData")
save(cat_dist_test, file = "../moderator_buster/cat_dist_test_convo.RData")

save(possible_categories, file = "../moderator_buster/possible_categories_convo.RData")
save(allowed_words, file = "../moderator_buster/allowed_words_convo.RData")