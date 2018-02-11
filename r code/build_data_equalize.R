#SVScarpino

#libraries
library(XML)

#acc functions
get_field <- function(x, field_name){
  return(x[[which(names(x) == field_name)]])
}

#iterating over entries
starts <- seq(0, (300000-10000), length.out = 30)
xml_data <- list()
for(i in starts){
  print(i)
  xml_call <- paste0("http://export.arxiv.org/api/query?search_query=all:the&start=", i, "&max_results=10000&sortBy=submittedDate&sortOrder=descending")
  data.i <- xmlParse(xml_call)
  xml_data.i <- xmlToList(data.i)
  xml_data <- c(xml_data, xml_data.i)
  Sys.sleep(3) #need to pause for 3 sec. as per arXiv API documentation
}


#extract category
entries <- which(names(xml_data) == "entry") #some meta data at the begining 
full_entries <- xml_data[entries]

primary_categories_list <- lapply(full_entries, FUN = get_field, field_name = "primary_category")
primary_categories_unlist <- unlist(primary_categories_list)
primary_categories <- primary_categories_unlist[which(names(primary_categories_unlist) == "entry.term")]

#extract summary
summaries_list <- lapply(full_entries, FUN = get_field, field_name = "summary")
summaries <- unlist(summaries_list)

#dataframe and saving
arxiv_2017_10k <- data.frame(primary_categories, summaries)
