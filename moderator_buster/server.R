#libraries
library(shiny)
library(keras)

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

#code
model <- load_model_hdf5("arxiv_2017_10k", custom_objects = NULL, compile = TRUE)
load("x_test.RData")
load("y_test.RData")
load("possible_categories.RData")
load("allowed_words.RData")
results <- model %>% evaluate(x_test, y_test)

shinyServer(function(input, output) {
  
  output$tab1 <- renderTable({
    tab <- data.frame(results$loss, results$acc)
    colnames(tab) <- c("OOS Loss", "OOS Acc")
    tab
  })
  
  template_selection_results <- eventReactive(input$Submit, {
    #1. remove punctuation
    no_new_line <- remove_punc(input$text, reg_ex = "\n")
    no_new_line_no_punc <- remove_punc(no_new_line, reg_ex = '[[:punct:]]')
    summaries_no_punc <- no_new_line_no_punc
    
    #2. filter on allowed words
    filtered_summaries <- filter_words(summaries_no_punc, allowed_words = allowed_words, replace_word = "STRONGCAT")
    
    #3. truncate/pad to 150 words
    trunc_fill_words <- trunc_pad(filtered_summaries, n_words = 150, filler_word = "STRONGCAT") #this is pretty fragile
    
    #4. create a matrix with the words
    x <- vectorize_sequences(sequences = trunc_fill_words, allowed_words = allowed_words, filler_word = "STRONGCAT") 
    
    #predict
    prediction <- model %>% predict(x)
    
    #return
    list("prediction" = prediction)
  })
    
  output$plot1 <- renderPlot({
    results <- template_selection_results()
    predictions <- results$prediction
    barplot(predictions[order(predictions, decreasing = TRUE)], names = possible_categories[order(predictions, decreasing = TRUE)], las = 2)
  })
  
})
