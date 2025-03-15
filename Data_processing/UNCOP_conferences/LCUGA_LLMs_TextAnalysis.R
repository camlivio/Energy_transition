#####################
###### LCUGA 2025
#####################
###### Camila Lívio 
#####################

######## the data 
library(tidyverse)
data <- read_csv(file.choose())

data$CleanedText[3] |> 
  print()

##### Here's our main problem: the 'Text' column is confusing and lengthy. 
##### One way to make sense of the content is to use the summarization function from the {mall} package to handle it for us. ##### I'm adding a new column for the summarized text and have set it to return 200 words.
library(dplyr)
library(mall)

#### **Make sure you install Ollama in your machine!**
#### https://hauselin.github.io/ollama-r/#installation

#### Start Ollama and choose a version


set.seed(123) ### R uses random numbers in many tasks, like selecting a sample, shuffling data, or working with machine learning models. Normally, every time you run the script, R generates different random results. Setting a seed (e.g., set.seed(123)) makes sure the "random" process gives the same results every time you run the code.

cop_text_summaries <- llm_summarize( 
  data, 
  CleanedText, 
  max_words = 200, 
  pred_name = "text_summary",
  additional_prompt = "Summarize the key decisions on energy transition from the Conference of the Parties reports, focusing on commitments, policies, and recommendations." 
) 

###### this operation takes a second! 

###### Apply predictions from the LLM to each row in a table (data frame). For each row, the model makes a prediction based on the information in that row and a pre-set question or prompt. It processes the rows one at a time.

###### check output 
cat(cop_text_summaries$text_summary[17])

###### now let's craft some code to prompt the LLM to extract keywords related to a certain phrase 

cop_text_summaries <- llm_extract(
  cop_text_summaries, #data
  text_summary, #column
  labels = "energy transition", #the entity to be extracted
  additional_prompt = "Return keywords related to energy transition.",
  pred_name = "extract_energy_trans" #new column
)

cop_text_summaries |> 
  select(extract_energy_trans)

library(ggplot2)

#### Count occurrences of each keyword
keyword_counts <- cop_text_summaries |>
  select(extract_energy_trans) |>
  separate_rows(extract_energy_trans, sep = ";") |>  
  count(extract_energy_trans, sort = TRUE)

#### Plot as a bar graph
energy_transition <- ggplot(keyword_counts, aes(x = reorder(extract_energy_trans, n), y = n)) +
  geom_col() +
  coord_flip() +  
  labs(title = "Keywords Related to *Energy Transition*",
       x = "Keyword",
       y = "Count") +
  theme_minimal()

energy_transition

####### validating the Results – our team has manually evaluated five texts so far

####### just out of curiosity: what’s the output when using word matching?
filtered_data <- data[grepl("energy transitions?", data$CleanedText), ]
filtered_data_2 <- data[grepl("transitions?", data$CleanedText), ]

####### This is particularly useful in text mining because it allows you to ask the model what to look for, rather than having to define the pattern from the start using word/phrase-matching techniques. 

####### In other words, you can skip the step of manually defining patterns and variations in your search. If we’re interested in learning about energy transition, we’re also interested in phrases like ‘decarbonization efforts’ or ‘shift to renewable energy.’

####### let's look for a different label 
cop_text_summaries <- llm_extract(
  cop_text_summaries, #data
  text_summary, #column
  labels = "renewable energy", #the entity to be extracted
  additional_prompt = "Return keywords related to renewable energy.",
  pred_name = "extract_renew_en" #new column
)

#### Count occurrences of each keyword
keyword_counts_re <- cop_text_summaries |>
  select(extract_renew_en) |>  
  drop_na(extract_renew_en) |>  
  separate_rows(extract_renew_en, sep = ";") |>  ### Split keywords into rows
  count(extract_renew_en, sort = TRUE)  ### Count occurrences

#### Plot as a bar graph
renewable_energy <- ggplot(keyword_counts_re, aes(x = reorder(extract_renew_en, n), y = n)) +
  geom_col() +
  coord_flip() +  # Flip for readability
  labs(title = "Keywords Related to *Renewable Energy*",
       x = "Keyword",
       y = "Count") +
  theme_minimal()

renewable_energy

####### Underlying Technology:
#### Both of these tasks (summarization and extraction) rely on deep learning techniques, specifically a type of neural network called a Transformer. Transformers are very good at understanding and generating language (Törnberg 2023, Ruiz 2024)

####### Next?
####### reproducibility 
####### validation 









