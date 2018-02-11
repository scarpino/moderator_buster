#SVScarpino

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
load("possible_categories.RData")
load("allowed_words.RData")
load("rich_prop_cor.RData")
load("results_OOS.RData")

shinyServer(function(input, output) {
  
  output$tab1 <- renderTable({
    tab <- data.frame(results_oos$loss, results_oos$acc)
    colnames(tab) <- c("OOS Loss", "OOS Acc")
    tab
  })
  
  output$plot2 <- renderPlot({
    barplot(rich_prop_cor[order(rich_prop_cor, decreasing = TRUE)], names = possible_categories[order(rich_prop_cor, decreasing = TRUE)], ylim = c(0,1), ylab = "OOS Accuracy", main = "OOS Accuracy by category", las = 2, cex.names = 0.8)
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
    names_full <- possible_categories[order(predictions, decreasing = TRUE)]
    names_trunc <- rep(NA, length(names_full))
    for(i in 1:length(names_full)){
      names_i <- unlist(strsplit(names_full[i], ":"))
      names_trunc[i] <- names_i[length(names_i)]
    }
    barplot(predictions[order(predictions, decreasing = TRUE)], names = , las = 2, ylab = "Proportion fit to category", main = "arXiv Category Classification")
  })
  
  output$results_text <- renderText({
    results <- template_selection_results()
    predictions <- results$prediction
    best_pred <- order(predictions, decreasing = TRUE)[1]
    if(predictions[best_pred] < 0.25){
      text_res <- "I wouldn't submit this to the arXiv"
    }else{
      text_res <- paste0("I would submit this to the ", possible_categories[best_pred], " category.")
    }
    text_res
  })
  
})
