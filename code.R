# Load necessary libraries
if (!require(tidyverse)) install.packages('tidyverse', dependencies = TRUE)
if (!require(data.table)) install.packages('data.table', dependencies = TRUE)
if (!require(tm)) install.packages('tm', dependencies = TRUE)
if (!require(tidytext)) install.packages('tidytext', dependencies = TRUE)
if (!require(ggplot2)) install.packages('ggplot2', dependencies = TRUE)
if (!require(lubridate)) install.packages('lubridate', dependencies = TRUE)
if (!require(topicmodels)) install.packages('topicmodels', dependencies = TRUE)
if (!require(slam)) install.packages('slam', dependencies = TRUE)
if (!require(widyr)) install.packages('widyr', dependencies = TRUE)
if (!require(ggraph)) install.packages('ggraph', dependencies = TRUE)
if (!require(igraph)) install.packages('igraph', dependencies = TRUE)
if (!require(scales)) install.packages('scales', dependencies = TRUE)
if (!require(syuzhet)) install.packages('syuzhet', dependencies = TRUE)
if (!require(text2vec)) install.packages('text2vec', dependencies = TRUE)
if (!require(Rtsne)) install.packages('Rtsne', dependencies = TRUE)
if (!require(httr)) install.packages('httr', dependencies = TRUE)

library(httr)
library(tidyverse)
library(data.table)
library(tm)
library(tidytext)
library(ggplot2)
library(lubridate)
library(topicmodels)
library(slam)
library(syuzhet)
library(widyr)
library(ggraph)
library(igraph)
library(scales)
library(text2vec)
library(Rtsne)

data(stop_words)

# Define the paths
download_path <- '../oldnewspapers/rda'
intermediate_data_dl_path <- '../oldnewspapers/data'
figs_path <- '../oldnewspapers/figs'

# Ensure directories exist
if (!dir.exists(download_path)) {
  dir.create(download_path, recursive = TRUE)
}
if (!dir.exists(intermediate_data_dl_path)) {
  dir.create(intermediate_data_dl_path, recursive = TRUE)
}
if (!dir.exists(figs_path)) {
  dir.create(figs_path, recursive = TRUE)
}

# Download the dataset from Kaggle
if (!file.exists(file.path(download_path, "old-newspaper.tsv.zip"))) {
  kaggle_api_url <- "https://www.kaggle.com/datasets/alvations/old-newspapers/download?datasetVersionNumber=1"
  
  # Read Kaggle API credentials
  kaggle_creds <- fromJSON("kaggle.json")
  kaggle_username <- kaggle_creds$username
  kaggle_key <- kaggle_creds$key
  
  # Set up the download URL with authentication
  download_url <- paste0("https://", kaggle_username, ":", kaggle_key, "@www.kaggle.com/api/v1/datasets/download/alvations/old-newspapers")
  
  # Download the dataset
  download.file(download_url, destfile = file.path(download_path, "old-newspaper.tsv.zip"))
}

# Unzip the dataset
unzip(file.path(download_path, "old-newspaper.tsv.zip"), exdir = download_path)

# Load the dataset into R
file_path <- file.path(download_path, "old-newspaper.tsv")
old_newspapers <- fread(file_path, sep = "\t", header = TRUE, quote = "")

# Filter for English articles
english_articles <- old_newspapers %>% filter(Language == "English")

# Save the filtered dataset for future use
write.csv(english_articles, file.path(intermediate_data_dl_path, "english_old_newspapers.csv"), row.names = FALSE)

# Load the filtered dataset into R
filtered_data_path <- file.path(intermediate_data_dl_path, "english_old_newspapers.csv")
english_articles <- fread(filtered_data_path)

# Check for missing values in the Text, Date, and Source fields
missing_summary <- english_articles %>% 
  summarise(
    total_rows = n(),
    missing_text = sum(is.na(Text) | Text == ""),
    missing_date = sum(is.na(Date) | Date == ""),
    missing_source = sum(is.na(Source) | Source == "")
  )

print(missing_summary)

# Remove rows with missing Text, Date, or Source values
english_articles_clean <- english_articles %>%
  filter(!is.na(Text) & Text != "" & 
           !is.na(Date) & Date != "" & 
           !is.na(Source) & Source != "")

# Save the cleaned dataset for future use
write.csv(english_articles_clean, file.path(intermediate_data_dl_path, "english_old_newspapers_clean.csv"), row.names = FALSE)

# Preview the cleaned dataset
head(english_articles_clean)

# Check the dimensions of the cleaned dataset
dim(english_articles_clean)
as_tibble(english_articles_clean)

# Text Normalization
normalize_text <- function(text) {
  text %>%
    tolower() %>%                             # Convert to lowercase
    str_replace_all("[[:punct:]]", " ") %>%   # Remove punctuation
    str_replace_all("[0-9]+", " ") %>%        # Remove numbers
    str_squish()                              # Strip excess whitespace
}

# Apply normalization to the Text column
english_articles_clean <- english_articles_clean %>%
  mutate(Text = map_chr(Text, normalize_text))

# Preview the normalized text
head(english_articles_clean$Text)

# Convert Date to Date type and add Year column
english_articles_clean <- english_articles_clean %>%
  mutate(Date = as.Date(Date, format = "%Y/%m/%d")) %>%
  mutate(Year = year(Date))

# Tokenization and stop words removal
tokenized_articles <- english_articles_clean %>%
  unnest_tokens(word, Text) %>%   # Tokenize the text into words
  anti_join(stop_words)   # Remove stop words

# Preview the tokenized data
head(tokenized_articles)


# Plot the distribution of articles over time with smoothing
temporal_distribution <- english_articles_clean %>%
  count(Date) %>%
  ggplot(aes(x = Date, y = n)) +
  geom_line(color = "blue") +
  geom_smooth(method = "loess", span = 0.1, color = "red") +
  labs(title = "Temporal Distribution of Articles",
       x = "Date",
       y = "Number of Articles") +
  theme_minimal(base_family = "Arial", base_size = 12) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_line(color = "grey90"))

# Save the plot
ggsave(file.path(figs_path, "temporal_distribution_smoothed.png"), plot = temporal_distribution)


# Analyze the number of articles by year
articles_by_year <- english_articles_clean %>%
  count(Year) %>%
  ggplot(aes(x = Year, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Number of Articles by Year",
       x = "Year",
       y = "Number of Articles") +
  theme_minimal(base_family = "Arial", base_size = 12) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_line(color = "grey90"))

# Save the plot
ggsave(file.path(figs_path, "articles_by_year.png"), plot = articles_by_year)

# Calculate word frequency and proportion
word_frequency <- tokenized_articles %>%
  count(word, sort = TRUE) %>%
  top_n(20, n) %>%
  mutate(proportion = n / sum(n))

# Plot the most common words with proportions
word_frequency_plot <- word_frequency %>%
  ggplot(aes(x = reorder(word, n), y = n, label = scales::percent(proportion, accuracy = 0.1))) +
  geom_bar(stat = "identity", fill = "red") +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)), 
            position = position_stack(vjust = 0.5), 
            color = "white", 
            size = 4) +
  coord_flip() +
  labs(title = "Top 20 Most Common Words",
       x = "Word",
       y = "Frequency") +
  theme_minimal(base_family = "Arial", base_size = 12) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_line(color = "grey90"))

# Save the plot
ggsave(file.path(figs_path, "word_frequency_proportion.png"), plot = word_frequency_plot)

# Perform sentiment analysis
english_articles_clean <- english_articles_clean %>%
  mutate(sentiment = get_sentiment(Text, method = "syuzhet"))

# Plot sentiment over time
sentiment_over_time <- english_articles_clean %>%
  group_by(Date) %>%
  summarise(avg_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  ggplot(aes(x = Date, y = avg_sentiment)) +
  geom_line(color = "blue") +
  geom_smooth(method = "loess", span = 0.1, color = "red", size = 0.5) +
  labs(title = "Average Sentiment Over Time",
       x = "Date",
       y = "Average Sentiment") +
  theme_minimal(base_family = "Arial", base_size = 12) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_line(color = "grey90"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot
ggsave(file.path(figs_path, "sentiment_over_time.png"), plot = sentiment_over_time)

# Add a unique identifier for each row
tokenized_articles <- tokenized_articles %>%
  mutate(document = row_number())

# Sentiment analysis using different lexicons
afinn_sentiments <- tokenized_articles %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(document) %>%
  summarize(afinn_sentiment = sum(value))

bing_sentiments <- tokenized_articles %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(document, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(bing_sentiment = positive - negative)

nrc_sentiments <- tokenized_articles %>%
  inner_join(get_sentiments("nrc"), by = "word", relationship = "many-to-many") %>%
  count(document, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(nrc_sentiment = positive - negative)

# Compare sentiment lexicons
sentiment_comparison <- english_articles_clean %>%
  mutate(rowid = row_number()) %>%
  left_join(afinn_sentiments, by = c("rowid" = "document")) %>%
  left_join(bing_sentiments, by = c("rowid" = "document")) %>%
  left_join(nrc_sentiments, by = c("rowid" = "document"))

# Save the comparison dataset
write.csv(sentiment_comparison, file.path(intermediate_data_dl_path, "sentiment_comparison.csv"), row.names = FALSE)

# Plot sentiment comparison
sentiment_comparison_plot <- sentiment_comparison %>%
  gather(key = "lexicon", value = "sentiment", afinn_sentiment, bing_sentiment, nrc_sentiment) %>%
  ggplot(aes(x = Date, y = sentiment, color = lexicon)) +
  geom_line() +
  labs(title = "Sentiment Comparison Over Time",
       x = "Date",
       y = "Sentiment") +
  theme_minimal(base_family = "Arial", base_size = 12) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_line(color = "grey90")) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

# Save the plot with increased width
ggsave(file.path(figs_path, "sentiment_comparison.png"), plot = sentiment_comparison_plot, width = 12, height = 6)

# Identify most common positive and negative words using AFINN lexicon
afinn_contributions <- tokenized_articles %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(occurrences = n(),
            contribution = sum(value)) %>%
  arrange(desc(contribution))

# Save the contributions dataset
write.csv(afinn_contributions, file.path(intermediate_data_dl_path, "afinn_contributions.csv"), row.names = FALSE)

# Plot most common positive and negative words
afinn_contributions_plot <- afinn_contributions %>%
  top_n(20, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(x = word, y = contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Most Common Positive and Negative Words (AFINN)",
       x = "Word",
       y = "Contribution") +
  scale_fill_manual(values = c("red", "green")) +
  theme_minimal(base_family = "Arial", base_size = 12) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_line(color = "grey90"))

# Save the plot
ggsave(file.path(figs_path, "afinn_contributions.png"), plot = afinn_contributions_plot)


# Create a Document-Term Matrix
dtm <- tokenized_articles %>%
  count(document = 1:nrow(tokenized_articles), word) %>%
  cast_dtm(document, word, n)

# Remove empty documents using the `slam` package
dtm <- dtm[slam::row_sums(dtm) > 0, ]

# Apply LDA with k = 5
lda_model <- LDA(dtm, k = 5, control = list(seed = 1234))

# Get top terms for each topic
topics <- tidy(lda_model, matrix = "beta")
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Plot the top terms for each topic
top_terms_plot <- top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 1) +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top Terms in Each Topic",
       x = "Term",
       y = "Beta") +
  theme_minimal(base_family = "Arial", base_size = 12) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_line(color = "grey90"),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(size = 12, face = "bold"))

# Save the plot
figs_path <- 'C:/Users/kroto/projects/oldnewspapers/figs'
ggsave(file.path(figs_path, "top_terms.png"), plot = top_terms_plot, width = 10, height = 8)

# Tokenize into bigrams
bigrams <- english_articles_clean %>%
  unnest_tokens(bigram, Text, token = "ngrams", n = 2)

# Separate bigrams into two words
bigrams_separated <- bigrams %>%
  separate(bigram, into = c("word1", "word2"), sep = " ")

# Filter out stop words
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Count bigrams
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# Create a network plot of bigrams with higher threshold
bigram_graph <- bigram_counts %>%
  filter(n > 500) %>%
  graph_from_data_frame()

# Create a more readable bigram network plot
bigram_plot <- ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), size = 5, repel = TRUE) +
  theme_void() +
  labs(title = "Bigram Network") +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5, size = 16)
  )

# Save the plot
figs_path <- 'C:/Users/kroto/projects/oldnewspapers/figs'
ggsave(file.path(figs_path, "bigram_network.png"), plot = bigram_plot, width = 12, height = 10)

# Word Embeddings
# Prepare the text data
text_data <- english_articles_clean$Text

# Tokenize the text data
tokens <- word_tokenizer(text_data)

# Create iterator over tokens
it <- itoken(tokens, progressbar = FALSE)

# Create vocabulary
vocab <- create_vocabulary(it)

# Create a vectorizer
vectorizer <- vocab_vectorizer(vocab)

# Create the term-co-occurrence matrix (TCM)
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5)

# Fit the GloVe model
glove <- GlobalVectors$new(rank = 50, x_max = 10)
word_vectors <- glove$fit_transform(tcm, n_iter = 20)

# Combine main and context vectors into word embeddings
word_vectors <- word_vectors + t(glove$components)

# Find words similar to "economy" more efficiently
economy_vector <- word_vectors["economy", , drop = FALSE]

# Calculate the cosine similarity between "economy" and all other words
similar_words <- sim2(x = word_vectors, y = economy_vector, method = "cosine", norm = "l2")

# Remove the word "economy" itself and get the top 10 similar words
similar_words <- sort(similar_words[,1], decreasing = TRUE)
similar_words <- similar_words[2:11]  # Exclude the word "economy" itself
similar_words

# Save the word vectors for future use
save(word_vectors, file = "word_vectors.RData")

# Load the trained Word2Vec model
load("word_vectors.RData")

# Shuffle the word vectors randomly
set.seed(123)  # For reproducibility
word_vectors <- word_vectors[sample(nrow(word_vectors)), ]

# Get a random sample of 500 word vectors for visualization
words_to_visualize <- word_vectors[1:500,]
word_labels <- rownames(words_to_visualize)

# Perform t-SNE
tsne_model <- Rtsne(words_to_visualize, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 500)
tsne_data <- as.data.frame(tsne_model$Y)
tsne_data$word <- word_labels

# Plot the t-SNE results
tsne_plot <- ggplot(tsne_data, aes(x = V1, y = V2, label = word)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_text(aes(label = word), size = 3, vjust = 1.5, hjust = 1.5) +
  theme_minimal(base_family = "sans", base_size = 11) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_line(color = "grey90"))
  labs(title = "t-SNE Visualization of Word Embeddings",
       x = "t-SNE Dimension 1",
       y = "t-SNE Dimension 2")

# Save and display the plot
ggsave(file.path(figs_path, "tsne_word_embeddings.png"), plot = tsne_plot, width = 12, height = 8)
print(tsne_plot)






