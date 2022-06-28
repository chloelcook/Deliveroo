install.packages('naniar', quiet = T)
install.packages('wordcloud', quiet = T)
install.packages('tm', quiet = T)
install.packages('tidytext', quiet = T)
install.packages('shinydashboard', quiet = T)
install.packages('tidyverse', quiet = T)
suppressMessages(library(tidyverse))
suppressMessages(library(stringr))
suppressMessages(library(naniar))
suppressMessages(library(visdat))
suppressMessages(library(wordcloud))
suppressMessages(library(tm))
suppressMessages(library(tidytext))
library(shinydashboard)
library(shiny)

# Read in data and set column types
df = read_csv('deliveroo.csv', col_select = c(loc_name, loc_number, description, review_count, review_rating, only_on_deliveroo, searched_category), 
              show_col_types = F)
df$searched_category = factor(df$searched_category) 
df$review_count = str_replace(df$review_count, "\\+", " ")
df$review_count = as.numeric(df$review_count)
df$loc_number = as.character(df$loc_number)

# Clean data (missing values)
df = df %>% filter(!is.na(review_rating), !is.na(loc_name))

# Add in variables of interest

# Review rating category
df = df %>% mutate(review_rating_cat = case_when((review_rating >= 0 & review_rating < 1) ~ '0-1', (review_rating >= 1 & review_rating < 2) ~ '1-2', 
                                                 (review_rating >= 2 & review_rating < 3) ~ '2-3', (review_rating >= 3 & review_rating < 4) ~ '3-4',
                                                 (review_rating >= 4 & review_rating <= 5) ~ '4-5'))

df$review_rating_cat = factor(df$review_rating_cat)

# Number of reviews category
df = df %>% mutate(review_count_cat = case_when((review_count >= 0 & review_count < 100) ~ '0-100', (review_count >= 100 & review_count < 200) ~ '100-200', 
                                                (review_count >= 200 & review_count < 300) ~ '200-300', (review_count >= 300 & review_count < 400) ~ '300-400',
                                                (review_count >= 400 & review_count <= 500) ~ '400-500'))

df$review_rating_cat = factor(df$review_rating_cat)

# Cuisine category - raw input
df$cuisine = sub(".*Serves ", "", df$description)
df$cuisine = sub("\\..*", "", df$cuisine)
df$cuisine = str_to_lower(df$cuisine)

# Dummy variables for top 20 cuisines
top_cuisines = df %>% select(cuisine) %>% unnest_tokens(output = word, input = cuisine) %>% count(word, sort = T) %>% filter(word != 'and') %>% head(20) 
top_cuisines_vector = top_cuisines$word

for(i in top_cuisines_vector) {                 
  new <- ifelse(str_detect(df$cuisine, paste0(i)), 1, 0)                     
  df[ , ncol(df) + 1] <- new               
  colnames(df)[ncol(df)] <- paste0(i)  
}

df$sumcuisines = rowSums(df[,c(top_cuisines_vector)])
df$other = ifelse((df$sumcuisines == 0), 1, 0)

head(df)
