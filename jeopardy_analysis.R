# Load necessary libraries
library(here)       # For setting the working directory
library(readr)      # For data import
library(dplyr)      # For data manipulation
library(tidyr)      # For data tidying
library(stringr)    # For string manipulation
library(ggplot2)    # For plotting

# Set the working directory to the location of your data
here("data", "jeopardy.csv")

# Import the Jeopardy data into an object called 'jeopardy'
jeopardy <- read_csv("data/jeopardy.csv")

# Explore the structure of the data
str(jeopardy)       # Print structure of the data
glimpse(jeopardy)   # Print a concise summary
View(jeopardy)      # View the data in a tabular format

# Print the first and last five rows
head(jeopardy)
tail(jeopardy)

# Print column names
names(jeopardy)

# Assign new names to a character vector
column_names <- c("show_number", "air_date", "round", "category", "value", "question", "answer")

# Assign new column names to our dataset
names(jeopardy) <- column_names

# value column has strings 
unique(jeopardy$value)

# filter out "None" from value column
jeopardy <- jeopardy %>%
  filter(value != "None") 

jeopardy <- filter(jeopardy, value!= "None")

# Clean up the 'value' column
jeopardy$value <- as.numeric(str_replace_all(jeopardy$value, c("\\$" = "", "," = "")))

glimpse(jeopardy)

# Count observations by 'round'
jeopardy %>%
  count(round)

# Separate the 'air_date' column into 'year', 'month', and 'day'
jeopardy <- jeopardy %>%
  separate_wider_delim(air_date, delim = "-", 
                       names = c("year", "month", "day")) %>%
  mutate_at(vars(year, month, day), as.numeric) %>%
  mutate(category = tolower(category))

# Hypothesis Testing ------------------------------------------------------

# Calculate the number of unique categories
length(unique(jeopardy$category))

# Set up variables for hypothesis testing
n_questions <- nrow(jeopardy)
prob_category_exp <- 1/3369
prob_not_category_exp <- 3368/3369
prob_expected <- c(prob_category_exp, prob_not_category_exp)

## Science ----
categories = pull(jeopardy, category)
n_science_categories <- sum(sapply(categories, function(c) "science" %in% c))

science_obs = c(n_science_categories, n_questions - n_science_categories)
chisq.test(science_obs, p = prob_expected)

## History ----
n_history_categories <- sum(sapply(categories, function(c) "history" %in% c))

history_obs = c(n_science_categories, n_questions - n_history_categories)

hist <- matrix(c(history_obs, prob_expected), ncol = 2)

chisq.test(history_obs, p = prob_expected)

# Unique Questions --------------------------------------------------------

# extract 'question' column from data set
questions <- pull(jeopardy, question)

# initializing an empty vector for later
terms_used <- character(0)

# loop to capture unique terms from each question
for( q in questions){
  # Split the sentence into distinct words
  split_sentence = str_split(q, " ")[[1]]
  
  # condition to only capture words greater than 6 characters
  for(term in split_sentence){
    if(!term %in% terms_used & nchar(term) >= 6){
      terms_used = c(terms_used, term)
    }
  }
}

glimpse(terms_used)

# Doing the same thing as the above for loop but *vectorization*
terms_used_new <- unique(unlist(strsplit(questions, " ")))
terms_used_new <- terms_used_new[nchar(terms_used_new) >= 6]

# Valuable Questions ------------------------------------------------------

values = pull(jeopardy, value)
value_count_data = NULL

for(term in terms_used_new[1:50]){
  n_high_value = 0
  n_low_value = 0
  
  for (i in 1:length(questions)){
    split_sentence = str_split(questions[i], " ")[[1]]
    
    if(term %in% split_sentence & values[i] >= 800){
      n_high_value = n_high_value + 1
    } else if (term %in% split_sentence & values[i] < 800){
      n_low_value = n_low_value + 1
    }
  }
  
  if(n_high_value >= 5 & n_low_value >= 5){
    # Perform a chi-squared test
    test <- chisq.test(c(n_high_value, n_low_value), p = c(2/5, 3/5))
    new_row <- c(term, n_high_value, n_low_value, test$p.value)
    
    value_count_data = rbind(value_count_data, new_row)
  }
  
}

# Create a tidy data frame from the value count data
tidy_value_count_data <- as_tibble(value_count_data)
colnames(tidy_value_count_data) <- c("term", "n_high", "n_low", "p_value")

# Filter and arrange the tidy data
tidy_value_count_data %>% 
  mutate_at(vars(n_high, n_low), as.numeric) %>%
  mutate(total_occur = n_high + n_low) %>%
  arrange(desc(total_occur), p_value) %>% 
  filter(p_value < 0.051) %>%
  View()


# Plotting ---------------------------------------------------------------

# Plot the number of questions per year
jeopardy %>%
  group_by(year) %>%
  summarize(questions = n()) %>%
  ggplot(aes(as.factor(year), questions, fill = as.factor(year))) + 
  geom_bar(stat = "identity") + theme_bw() + scale_fill_viridis_d() +
  guides(fill = guide_legend(title = "Year")) +
  labs(title = "No. of Questions per Year", x = "Year",  y = "No. of Questions")
