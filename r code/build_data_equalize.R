#SVScarpino

#libraries
library(XML)
library(httr)

#acc functions
get_field <- function(x, field_name){
  return(x[[which(names(x) == field_name)]])
}

#getting categories
categories_xml <- xmlParse("http://export.arxiv.org/oai2?verb=ListSets")
categories_raw <- xmlToList(categories_xml)
categories <- c()
for(i in 1:length(categories_raw$ListSets)){
  categories <- c(categories, categories_raw$ListSets[[i]]$setSpec)
}

#iterating over entries
arxiv_2017_equalize <- list()
for(i in categories){
  print(i)
  xml_call <- paste0("http://export.arxiv.org/oai2?verb=ListRecords&from=2017-01-01&metadataPrefix=arXivRaw&set=",i)
  raw_results_i <- GET(url = xml_call)
  text_results_i <- rawToChar(raw_results_i$content)
  
  if(length(grep("Retry after 10 seconds", text_results_i)) > 0){
    Sys.sleep(10) #need to pause for 10 sec. per request
    raw_results_i <- GET(url = xml_call)
    text_results_i <- rawToChar(raw_results_i$content)
  }
  
  xml_data_raw.i <- xmlToList(text_results_i)
  full_entries.i <- xml_data_raw.i$ListRecords
  
  primary_categories_i <- rep(NA, (length(full_entries.i)-1))#last entry is just meta data
  summaries_i <- primary_categories_i
  full_categories_i <- primary_categories_i
  
  for(j in 1:(length(full_entries.i)-1)){ #last entry is just meta data
    primary_categories_i[j] <- full_entries.i[[j]]$header$setSpec
    summaries_i[j] <- full_entries.i[[j]]$metadata$arXivRaw$abstract
    full_categories_i[j] <- full_entries.i[[j]]$metadata$arXivRaw$categories
  }
  
  #dataframe and saving
  arxiv_2017_i<- data.frame(primary_categories_i, summaries_i, full_categories_i)
  
  arxiv_2017_equalize[[i]] <- arxiv_2017_i
  
  Sys.sleep(10) #need to pause for 10 sec. as per arXiv API documentation
}

save(arxiv_2017_equalize, file = paste0("../data/", as.numeric(Sys.time()), "_arxiv_2017_equalize.RData"))