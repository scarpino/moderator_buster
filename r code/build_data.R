#SVScarpino

#libraries
library(XML)

#acc functions
get_field <- function(x, field_name){
  return(x[[which(names(x) == field_name)]])
}

#data
data <- xmlParse("data/arxiv_2017_10000.xml")
xml_data <- xmlToList(data)

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
save(arxiv_2017_10k, file = paste0(as.numeric(Sys.time()), "arxiv_2017_10k.RData"))
