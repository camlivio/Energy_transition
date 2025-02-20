####################

library(tidyverse)
file.choose()
cop <- read_csv("/Users/camilalivio/Desktop/cop_summaries.csv")

library(dplyr)
library(stringr)
library(ggplot2)

library(dplyr)
library(stringr)
library(ggplot2)

# Extract the event number from the Document column and order the factor levels
cop$EventNumber <- as.integer(str_extract(cop$Document, "\\d+"))

# Count occurrences of "transition" by Document
by_document_count <- cop %>%
  group_by(Document) %>%
  summarise(transition_count = sum(str_count(CleanedText, "\\btransition\\b"))) %>%
  # Order by EventNumber
  mutate(Document = factor(Document, levels = unique(Document[order(cop$EventNumber)])))

# Plot the counts by Document
# Plot the counts by Document with labels on top of each bar
ggplot(by_document_count, aes(x = Document, y = transition_count)) +
  geom_bar(stat = "identity", fill = "#09716F") +  # Add color to the bars
  geom_label(aes(label = transition_count), vjust = 0) +  # Add count labels on top
  theme_minimal() +
  labs(
    title = "Counts of 'Transition' by COP Event",
    subtitle = "n = 115",
    x = "COP Event",
    y = "Count"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


overall_transition_count <- sum(str_count(cop$CleanedText, "\\btransition\\b"))
overall_transition_count

library(quanteda)
library(dplyr)
# Create a tokens object from the CleanedText column
tokens_corpus <- tokens(corpus)

# Find concordance lines with the term "transition"
concordance_lines <- kwic(tokens_corpus, pattern = "transition", window = 5)
concordance_lines

library(tidytext)
library(dplyr)
library(stringr)

# Convert CleanedText into a tidy data frame
cop_tidy <- cop %>%
  unnest_tokens(word, CleanedText)

# Create four-grams (four-word phrases) from the text
cop_fourgrams <- cop_tidy %>%
  mutate(next_word1 = lead(word), 
         next_word2 = lead(word, 2),
         next_word3 = lead(word, 3)) %>%
  filter(!is.na(next_word3)) %>%
  unite(fourgram, word, next_word1, next_word2, next_word3, sep = " ")

# Find four-grams related to "transition"
transition_fourgrams <- cop_fourgrams %>%
  filter(str_detect(fourgram, "transition"))

# Count the frequency of each four-gram
fourgram_freq <- transition_fourgrams %>%
  count(fourgram, sort = TRUE)

# View the most frequent four-grams
head(fourgram_freq)

library(ggplot2)

# Get the top 20 most frequent four-grams
top_20_fourgrams <- head(fourgram_freq, 20)

# Plot the top 20 most frequent four-grams
ggplot(top_20_fourgrams, aes(x = reorder(fourgram, -n), y = n)) +
  geom_bar(stat = "identity", fill = "#09716F") +  # Bar plot with custom color
  theme_minimal() +
  labs(
    title = "Top 20 Most Frequent Four-grams Related to 'Transition'",
    x = "Four-gram",
    y = "Frequency"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

library(tidytext)
library(dplyr)
library(stringr)

# Load stop words from tidytext
data("stop_words")

# Convert CleanedText into a tidy data frame
cop_tidy <- cop %>%
  unnest_tokens(word, CleanedText)

# Find positions where "transition" occurs and create a context window
transition_positions <- cop_tidy %>%
  filter(word == "transition") %>%
  mutate(position = row_number())

# Create a context window of 5 words around "transition"
context_window <- cop_tidy %>%
  mutate(position = row_number()) %>%
  filter(abs(position - transition_positions$position) <= 5 & word != "transition")

# Remove stop words
context_window_filtered <- context_window %>%
  anti_join(stop_words, by = "word")

# Count co-occurrences of words within the window after removing stop words
collocations <- context_window_filtered %>%
  count(word, sort = TRUE)

# View the collocations
head(collocations)

# Load necessary libraries
library(tm)           # Text mining package
library(SnowballC)    # For stemming
library(dplyr)        # For data manipulation
library(tidyr)        # For reshaping data
library(wordcloud)    # For word cloud
library(tidytext)     # For text mining
library(ggplot2)      # For visualization

# Load the dataset
df <- cop  # Assuming 'cop' is your dataset

# Preprocess the text
corpus <- Corpus(VectorSource(df$Text))

# Clean the corpus (remove stopwords, punctuation, and numbers)
corpus_clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>%    # Convert to lowercase
  tm_map(removePunctuation) %>%                # Remove punctuation
  tm_map(removeNumbers) %>%                   # Remove numbers
  tm_map(removeWords, stopwords("en")) %>%     # Remove stopwords
  tm_map(stripWhitespace)                      # Remove extra whitespace

# Tokenize the text (into words)
dtm <- DocumentTermMatrix(corpus_clean)

# Convert DTM to a matrix and calculate word frequencies
word_freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
word_freq_df <- data.frame(word = names(word_freq), freq = word_freq)

# Visualize the most frequent words using a bar plot
top_words <- head(word_freq_df, 20)  # Get the top 10 words
ggplot(top_words, aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "#09716F") +
  coord_flip() +
  labs(title = "Top 10 Most Frequent Words", x = "Word", y = "Frequency")

# For phrases, you can extract n-grams (bigrams, trigrams, etc.)
bigram_tokens <- df %>%
  unnest_tokens(bigram, CleanedText, token = "ngrams", n = 2)

bigram_freq <- bigram_tokens %>%
  count(bigram, sort = TRUE)

# Visualize the most frequent bigrams
top_bigrams <- head(bigram_freq, 10)
ggplot(top_bigrams, aes(x = reorder(bigram, n), y = n)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  coord_flip() +
  labs(title = "Top 10 Most Frequent Bigrams", x = "Bigram", y = "Frequency")

# Optionally, create a word cloud for visualizing frequent terms
wordcloud(words = word_freq_df$word, freq = word_freq_df$freq, min.freq = 150, colors = "#09716F")

