library(pdftools)
library(dplyr)

#### Folder path -- grab from COP_reports folder in my desktop
#### I have initially collected the reports from 1995-2000 for exploration
folder_path <- "~/Desktop/COP_reports/final_reports"

#### Match PDF files with regexp 
pdf_files <- list.files(path = folder_path, pattern = "^[A-Za-z]+_[0-9]+\\.pdf$", full.names = TRUE)

#### Making sure we grabbed all the files 
print(pdf_files)

#### Initialize an empty data frame
pdf_data <- data.frame(Document = character(), Text = character(), stringsAsFactors = FALSE)

#### Loop through the PDF files to extract text
if (length(pdf_files) > 0) {
  for (file in pdf_files) {
    text <- pdf_text(file) %>% 
      paste(collapse = " ")  
    
    pdf_data <- pdf_data %>%
      add_row(Document = basename(file), Text = text)
  }
} else {
  print("No matching files found")
}

#### Let's have a look!
glimpse(pdf_data)

#### great -- needs lotsa cleaning though!
library(tidytext)
library(textstem)

#### Let's chatGPT a text cleaning pipeline
pdf_data <- pdf_data %>%
  mutate(
    CleanedText = tolower(Text),                               # Lowercase
    CleanedText = gsub("[[:punct:]]", " ", CleanedText),       # Remove punctuation
    CleanedText = gsub("[[:digit:]]", "", CleanedText),        # Remove numbers
    CleanedText = gsub("\\s+", " ", CleanedText),             # Remove extra spaces
    CleanedText = trimws(CleanedText),                        # Trim whitespace
    CleanedText = lemmatize_strings(CleanedText)              # Lemmatize text
  ) 

colnames(pdf_data)


############################################################################
########### Let's look for some patterns using corpus linguistics techniques 
############################################################################

library(quanteda)
library(dplyr)
library(tidyverse)

file.choose()
data <- read_csv("/Users/camilalivio/Desktop/cop_summaries.csv")

library(quanteda)
library(dplyr)

### convert "cop_summary" to lowercase for case-insensitive matching
data <- data %>%
  mutate(cop_summary = tolower(cop_summary))

### create a corpus from the "cop_summary" column
corpus_data <- corpus(data, text_field = "cop_summary")

### tokenize the corpus (remove punctuation)
tokens_data <- tokens(corpus_data, remove_punct = TRUE)

### find contexts for renewable, wind, and solar energy/power terms
kwic_renewable <- kwic(tokens_data, pattern = phrase(c("renewable energy", "renewable power")), window = 5)

### print concordance lines
print(kwic_renewable)


