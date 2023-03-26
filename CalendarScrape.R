##################################  INSTALL PACKAGES  ##################################
install.packages('pdftables')
install.packages('pdftools')
install.packages('tabulizer')
install.packages('pdftools')
install.packages('stringr')
install.packages('rvest')
install.packages('tidyverse')

##################################  LIBRARIES     ##################################

library(rvest)
library(tidyverse)
library(stringr)
library(pdftables)
library(pdftools)
library(tabulizer)

setwd("C:/Users/Vic/Desktop/Queensland Treasury/Code/R/CalendarScrape")

##################################  SOURCE FILES  ##################################

# Melbourne Institute - Latest News URL
miURL = 'https://melbourneinstitute.unimelb.edu.au/publications/macroeconomic-reports/latest-news/index-of-consumer-sentiment'

# Westpac PDF
pdfFile = 'Economic_Calendar_March2023_WBC.pdf'



##################################  URL SCRAPING  ##################################
####------------------------------  rvest         ------------------------------####

westpacDate <- read_html(miURL)
westpacDate
date <- westpacDate %>% html_elements("p") %>% html_text()

finalDate <- sub('.*will be released at ', '', date[str_detect(date, "will be released at")])
print(finalDate)


##################################  PDF SCRAPING  ##################################
####------------------------------  tabulizer     ------------------------------####


# function to extract table data from PDF
extract_table <- function(filename) {
  # read in PDF file
  pages <- extract_tables(filename, pages = 1:3)
  
  # initialize empty dataframe
  df <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(df) <- c("day", "date", "event")
  
  # loop through pages and rows to populate dataframe
  for (page in 1:length(pages)) {
    n_rows <- nrow(pages[[page]])
    for (i in seq(n_rows, 2, -2)) {
      # Check if current page has 3 rows
      if (n_rows == 3) {
        date <- pages[[page]][1, i:(i+1)]
        event <- pages[[page]][3, i:(i+1)]
      } else {
        date <- pages[[page]][2:3, i:(i+1)]
        event <- pages[[page]][4:5, i:(i+1)]
      }
      # Add date and event to dataframe
      df <- rbind(df, data.frame(
        day = days[(i-1)/2],
        date = paste(date[1,], date[2,], sep = "-"),
        event = paste(event[1,], event[2,], sep = "\n")
      ))
    }
  }
  
  
  # return final dataframe
  return(df)
}

df <- extract_table(pdfFile)



####------------------------------  pdftools  v2.0    ------------------------------####

library(pdftools)
library(stringr)

# read the pdf file into R
pdf_text <- pdf_text(pdfFile)

# find the start and end page of the table
start_page <- grep("MON\\s+TUE\\s+WED\\s+THU\\s+FRI", pdf_text)
end_page <- min(grep("Contacts", pdf_text)) - 1

# loop through the pages with the table
for (page in start_page:end_page) {
  # extract the table data from the page
  
  table_text <- str_extract_all(pdf_text[page], "(?<=\n)\\b\\w+\\s+\\d{4}\\b.*?(?=\\b\\w+\\s+\\d{4}\\b|$).{1,49}", simplify = TRUE)
  print(table_text)
  
  # split the table text into columns
  table_data <- matrix(table_text, ncol = 5, byrow = TRUE)
}


####------------------------------  pdftools v1.0    ------------------------------####

# Load packages
library(pdftools)
library(stringr)

# Extract PDF text
pdf_text <- pdf_text(pdfFile)

pdf_text

# Identify pages with table
table_pages <- which(str_detect(pdf_text, "MON"))

# Extract table text from each page
table_text <- sapply(table_pages, function(page) {
  str_extract(pdf_text[page], "(?s)MON.*?\\n\\n")
})

# Combine table text into a single string
table_text <- paste(table_text, collapse = "")

# Convert table text to data frame
table_data <- read.table(text = table_text, header = FALSE, stringsAsFactors = FALSE)

# Rename columns
names(table_data) <- c("MON", "TUE", "WED", "THU", "FRI")

# Split rows into separate columns
table_data <- t(apply(table_data, 1, function(x) {
  date <- str_split(x[2], " ")[[1]][2]
  events <- paste(x[3:length(x)], collapse = " ")
  c(date, events)
}))

# Convert to data frame
table_data <- as.data.frame(table_data, stringsAsFactors = FALSE)

# Remove rows after table
table_data <- table_data[1:5,]

# Use table_data for further analysis


####------------------------------  convert_pdf      ------------------------------####

convert_pdf(pdfFile, output_file = NULL, message = TRUE, api_key = Sys.getenv("pdftable_api"))
