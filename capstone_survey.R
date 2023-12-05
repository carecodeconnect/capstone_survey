options(warn = -1)

Sys.setenv(OPENSSL_CONF="/dev/null")

options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Function to check and install packages
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

# Check and load necessary libraries
packages <- c("here", "tidyverse", "tm", "SnowballC", "tidytext", "dplyr", "ggplot2", "knitr", "kableExtra", "tidymodels", "ranger", "tune", "car", "forcats", "yardstick", "RColorBrewer")

lapply(packages, install_if_missing)

# Import SPSS data into R and select only the variables of interest
#survey <- haven::read_sav(here("survey.sav")) %>%
#  mutate(
#    motivations = Q5.2,
#    avenues_txt = Q5.1,
#    outlook_txt = Q5.4,
#    age = Q6.1,
#    gender = Q6.2,
#    sexuality = Q6.3,
#    ethnicity = R6.4,
#    disability = Q6.5,
#    technology = R2.25,
#    website = Q7.2,
#    online_teaching = Q2.13,
#    online_n = Q2.14,
#    year_started = Q2.10
#  ) %>%
#  select(motivations, avenues_txt, outlook_txt, age, gender, #sexuality, ethnicity, disability, technology, website, #online_teaching, online_n, year_started)

# Transforming the variables
#survey <- survey %>%
#  mutate(
#    motivations = na_if(motivations, ""),
#    avenues_txt = na_if(avenues_txt, ""),
#    outlook_txt = na_if(outlook_txt, ""),
#    age = as_factor(age),
#    gender = as_factor(gender),
#    sexuality = as_factor(sexuality),
#    ethnicity = as_factor(ethnicity),
#    disability = as_factor(disability),
#    technology = as_factor(technology),
#    website = as_factor(website),
#    online_teaching = as_factor(online_teaching),
#    online_n = as_factor(online_n),
#    year_started = {
#      temp <- na_if(year_started, "")
#      temp <- as.numeric(temp)
#      attr(temp, "label") <- attr(survey$year_started, "label")
#      temp
#    }
#  )

# Save the modified data frame
#save(survey, file = "modified_survey.RData")

# Load the saved data frame
load("modified_survey.RData")

# Define the preprocessing function
preprocess_text <- function(text) {
  # Convert to lower case
  text <- tolower(text)
  
  # Remove punctuation
  text <- removePunctuation(text)
  
  # Remove stopwords
  text <- removeWords(text, stopwords("english"))
  
  # Tokenize and stem words (optional)
  text <- wordStem(strsplit(text, " ")[[1]])
  
  # Rejoin the words
  text <- paste(text, collapse = " ")
  
  return(text)
}

# Apply the preprocessing function to create new variables
survey$motivations_pp <- sapply(survey$motivations, preprocess_text, USE.NAMES = FALSE)
survey$avenues_txt_pp <- sapply(survey$avenues_txt, preprocess_text, USE.NAMES = FALSE)
survey$outlook_txt_pp <- sapply(survey$outlook_txt, preprocess_text, USE.NAMES = FALSE)


# Convert your text data to a data frame if it's not already
text_data <- data.frame(text = survey$outlook_txt, stringsAsFactors = FALSE)

# Tokenize the data
tokenized_data <- text_data %>%
  unnest_tokens(word, text)

# Print sample of tokenized data
#print(head(tokenized_data))

# Get sentiment scores
sentiment_scores <- tokenized_data %>%
  inner_join(get_sentiments("bing"), by = "word")

# Print sample of sentiment scores
#print(head(sentiment_scores))

# Summarize sentiment scores
sentiment_summary <- sentiment_scores %>%
  group_by(sentiment) %>%
  summarise(count = n()) %>%
  spread(sentiment, count, fill = 0)

# Print sentiment summary
#print(sentiment_summary)

# Assuming 'survey$gender' contains the gender data
gender_data <- table(survey$gender)

# Create a data frame for better presentation in kable
gender_df <- data.frame(
  Gender = names(gender_data),
  Count = as.vector(gender_data)
)

# Use kable to create a table with simple formatting and added styling
kable(gender_df, 
      format = "simple", 
      caption = "Gender Distribution", 
      booktabs = TRUE, 
      row.names = FALSE)

# Define your categorical variables
factor_vars <- c("age", "gender", "sexuality", "ethnicity", "disability", "technology", "website", 
                 "online_teaching", "online_n", "formal_education", "prof_quals", "supervise_emp", 
                 "employment_other_type", "trained", "trained_n", "advocacy", 
                 "business_entrepreneurial", "management", "courses_in_year", "clients_taught", 
                 "independent", "nations", "region_england", "region_wales", "region_scotland", 
                 "region_ireland", "areas", "contexts")

# Function to create a bar chart
create_bar_chart <- function(data, var_name, flip_axes = FALSE) {
  # Filter out NA values
  data <- data %>% filter(!is.na(.[[var_name]]))
  
  # Create a ggplot object
  if (!flip_axes) {
    p <- ggplot(data = data, aes_string(x = var_name, fill = var_name)) +
      geom_bar(show.legend = FALSE) +
      labs(title = paste("Bar chart of", var_name), x = NULL, y = "Frequency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed
  } else {
    p <- ggplot(data = data, aes(y = var_name, fill = var_name)) +
      geom_bar(show.legend = FALSE) +
      labs(title = paste("Bar chart of", var_name), x = "Frequency", y = NULL) +
      theme_minimal() +
      coord_flip()  # Flips the axes for a horizontal bar chart
  }
  
  # Get the number of levels for the current factor variable
  num_levels <- nlevels(as.factor(data[[var_name]]))
  
  # Choose a color palette that can accommodate the number of levels
  colors <- colorRampPalette(brewer.pal(min(max(3, num_levels), 9), "Set3"))(num_levels)  # Adjust if you have more than 9 levels
  
  # Apply the color palette to the plot
  p <- p + scale_fill_manual(values = colors)
  
  return(p)
}

# Generate and display each plot individually

# For 'age'
create_bar_chart(survey, "age")

# Convert data to long format for faceting
survey_long <- tidyr::gather(survey, key="demographic", value="value", ethnicity, sexuality, disability)

# Calculate frequency for each value within each demographic and arrange them
survey_freq <- survey_long %>%
  dplyr::group_by(demographic, value, .drop = FALSE) %>%
  dplyr::summarise(freq = n(), .groups = "drop_last") %>%
  dplyr::ungroup()

# Arrange by demographic and frequency and set factor levels
survey_freq <- survey_freq %>%
  dplyr::group_by(demographic) %>%
  dplyr::mutate(value = factor(value, levels=rev(unique(value[order(freq)])))) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(demographic, desc(freq))

# Remove NA values
survey_freq <- survey_freq %>%
  dplyr::filter(!is.na(value))

# Rename "Non-white" to "People of colour" and adjust the factor levels
survey_freq <- survey_freq %>%
  dplyr::mutate(value = ifelse(demographic == "ethnicity" & value == "Non-white", "People of colour", as.character(value)))

# Convert the 'value' column back to a factor after the renaming
survey_freq$value <- factor(survey_freq$value)

# Plot
p_combined <- ggplot(survey_freq, aes(x=freq, y=value, fill=value)) + 
  geom_bar(stat="identity") + 
  facet_grid(demographic ~ ., scales="free_y") +
  labs(title="Distribution of Identities") +
  theme(axis.text.y = element_text(hjust = 1), legend.position = "none") + 
  scale_fill_brewer(palette="Set1")

p_combined

# Selecting only relevant variables for modeling
relevant_variables <- survey[, c("age", "gender", "online_teaching", "year_started")]

# Convert blank to NA for relevant variables
relevant_variables$age[relevant_variables$age == ""] <- NA
relevant_variables$gender[relevant_variables$gender == ""] <- NA
relevant_variables$online_teaching[relevant_variables$online_teaching == ""] <- NA
relevant_variables$year_started[relevant_variables$year_started == ""] <- NA

# Count NAs in relevant variables pre-filter
na_count <- sum(is.na(relevant_variables))
#print(paste("Number of NA values in relevant variables:", na_count))

# Filter dataset for NA values in key variables
survey_filtered <- relevant_variables %>%
  filter(!is.na(age) & !is.na(gender) & !is.na(online_teaching) & !is.na(year_started))

# Check for NA values in the filtered dataset
#print(paste("Number of NAs in the filtered dataset:", sum(is.na(survey_filtered))))

# Stratified splitting of the dataset based on 'gender'
set.seed(123)
data_split <- initial_split(survey_filtered, prop = 0.8, strata = "gender")
train_data <- training(data_split)
test_data  <- testing(data_split)

# Check the number of levels for each factor variable
#factor_levels <- sapply(train_data, function(x) if(is.factor(x)) length(levels(x)))
#print(factor_levels)

# Preprocessing the data
data_recipe <- recipe(online_teaching ~ age + gender + year_started, data = train_data) %>%
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes())

# Random Forest Model specification
rf_model <- rand_forest(mode = "classification") %>%
  set_engine("ranger") %>%
  set_mode("classification")

# Cross-validation setup
cv <- vfold_cv(train_data, v = 5, strata = "gender")

# Workflow for Random Forest
rf_workflow <- workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(rf_model)

# Fit and evaluate Random Forest Model
rf_results <- try(fit_resamples(
  rf_workflow,
  resamples = cv,
  metrics = metric_set(roc_auc, accuracy)
), silent = TRUE)

# Check errors in fitting Random Forest Model
if (inherits(rf_results, "try-error")) {
  print("Error in fitting Random Forest Model")
} else {
  rf_performance <- rf_results %>% collect_metrics()
  #print(rf_performance)
}

# Plotting accuracy and ROC AUC
ggplot(rf_performance, aes(x = .metric, y = mean, fill = .metric)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Random Forest Model Performance Metrics",
       x = "Metric",
       y = "Mean Value") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")


rf_performance_table <- data.frame(
  Metric = c("accuracy", "roc_auc"),
  Estimator = c("binary", "binary"),
  Mean = c(0.7408159, 0.5639125),
  N = c(5, 5),
  `Standard Error` = c(0.01790040, 0.02160701)
)

# Generate the table
rf_performance_table %>%
  kable("simple", booktabs = TRUE, caption = "Random Forest Model Performance")

# Chi-squared test for Gender and Technology Use
chisq_test_gender_technology <- chisq.test(survey$gender, survey$technology)
print(chisq_test_gender_technology)

# Chi-squared test for Gender and Online Teaching
chisq_test_gender_online_teaching <- chisq.test(survey$gender, survey$online_teaching)
print(chisq_test_gender_online_teaching)

# Chi-squared test for Gender and Number of Students Taught Online
# (Assuming 'online_n' is categorical. If it's continuous, it needs a different approach)
chisq_test_gender_online_n <- chisq.test(survey$gender, survey$online_n)
print(chisq_test_gender_online_n)

# Cross-tabulation for 'gender' and 'online_teaching'
gender_online_teaching_crosstab <- table(survey$gender, survey$online_teaching)
gender_online_teaching_crosstab

# Proportions for 'gender' and 'online_teaching', across each level of 'gender'
prop_gender_online_teaching <- prop.table(gender_online_teaching_crosstab, 1) * 100  # Proportions in percentage
prop_gender_online_teaching

# Cross-tabulation for 'gender' and 'online_n'
gender_online_n_crosstab <- table(survey$gender, survey$online_n)
gender_online_n_crosstab

# Proportions for 'gender' and 'online_n', across each level of 'gender'
prop_gender_online_n <- prop.table(gender_online_n_crosstab, 1) * 100  # Proportions in percentage
prop_gender_online_n

# Creating the data with direct column naming
simplified_table_data <- data.frame(
  "Tested Relationship" = c("Gender and Technology", 
                            "Gender and Online Teaching", 
                            "Gender and Online N"),
  "X-squared (Test statistic)" = c(1.7838, 14.204, 35.323),
  "Degrees of Freedom (df)" = c(3, 3, 18),
  "p-value" = c(0.6185, 0.00264, 0.008605),
  "Significance" = ifelse(c(0.6185, 0.00264, 0.008605) < 0.05, "Significant", "Not Significant")
)

# Displaying the revised table using kable with simple formatting
kable(simplified_table_data, 
      caption = "Results for Pearson's Chi-Squared Tests", 
      format = "simple", 
      booktabs = TRUE, 
      row.names = FALSE)



# 1. Gender and Online Teaching Preference Table
teaching_data <- data.frame(
  Gender = c("Female", "Male", "Other, please specify", "Prefer not to say"),
  Yes = c(105, 60, 4, 2),
  No = c(356, 125, 1, 4),
  `Yes Percentage` = c(22.78, 32.43, 80.00, 33.33),
  `No Percentage` = c(77.22, 67.57, 20.00, 66.67)
)

# Display the teaching preference table using kable with simple formatting
kable(teaching_data, 
      caption = "Gender and Online Teaching Preference", 
      format = "simple", 
      booktabs = TRUE, 
      row.names = FALSE)

# 2. Gender and Number of Students Taught Online Table
students_data <- data.frame(
  Gender = c("Female", "Male", "Other, please specify", "Prefer not to say"),
  Zero = c(303, 106, 1, 4),
  `1-5` = c(53, 11, 0, 0),
  `6-10` = c(21, 10, 1, 0),
  `11-20` = c(10, 14, 1, 1),
  `21-50` = c(21, 11, 1, 0),
  `51-100` = c(19, 11, 0, 0),
  `More than 100` = c(34, 22, 1, 1),
  `Zero Percentage` = c(65.73, 57.30, 20.00, 66.67),
  `1-5 Percentage` = c(11.50, 5.95, 0.00, 0.00),
  `6-10 Percentage` = c(4.56, 5.41, 20.00, 0.00),
  `11-20 Percentage` = c(2.17, 7.57, 20.00, 16.67),
  `21-50 Percentage` = c(4.56, 5.95, 20.00, 0.00),
  `51-100 Percentage` = c(4.12, 5.95, 0.00, 0.00),
  `More than 100 Percentage` = c(7.38, 11.89, 20.00, 16.67)
)

# Display the students taught online table using kable with simple formatting
kable(students_data, 
      caption = "Gender and Number of Students Taught Online", 
      format = "simple", 
      booktabs = TRUE, 
      row.names = FALSE)


# Data preprocessing
survey <- survey %>%
  mutate(gender = fct_na_value_to_level(gender),  # Convert "" to NA in factor column
         online_teaching = as.character(online_teaching),  # Convert to character temporarily
         online_teaching = na_if(online_teaching, "")) %>%
  drop_na(gender, online_teaching)

# Convert online_teaching back to factor
survey$online_teaching <- as.factor(survey$online_teaching)

# Selecting only relevant variables for modeling
relevant_variables <- survey[, c("gender", "online_teaching")]

# Count NAs in relevant variables pre-filter
na_count <- sum(is.na(relevant_variables))
#print(paste("Number of NA values in relevant variables:", na_count))

# Filter dataset for NA values in key variables
survey_filtered <- relevant_variables %>%
  filter(!is.na(gender) & !is.na(online_teaching))

# Check for NA values in the filtered dataset
#print(paste("Number of NAs in the filtered dataset:", sum(is.na(survey_filtered))))

# Splitting the data with a set seed for reproducibility
set.seed(123)
split <- initial_split(survey, prop = 0.8, strata = gender)
train_data <- training(split)
test_data <- testing(split)

# Check the structure of the training and test data sets
#print("Training data structure:")
#print(str(train_data))
#print("Test data structure:")
#print(str(test_data))

# Model preparation with error handling
rf_spec <- rand_forest(mtry = 2, trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("classification")

xgb_spec <- boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# Workflow creation and fitting models with error handling
workflow_list <- list(Random_Forest = rf_spec, XGBoost = xgb_spec)

fits <- list()
for (model_name in names(workflow_list)) {
  spec <- workflow_list[[model_name]]
  message("Fitting ", model_name)
  fit_result <- try(
    workflow() %>%
      add_formula(online_teaching ~ gender) %>%
      add_model(spec) %>%
      fit(data = train_data),
    silent = TRUE
  )
  
  if (inherits(fit_result, "try-error")) {
    message("Error fitting ", model_name)
  } else {
    fits[[model_name]] <- fit_result
    print(fit_result)
  }
}

# Prediction and metrics with error handling
for(model_name in names(fits)) {
  fit <- fits[[model_name]]
  if (!is.null(fit)) {
    message("Predicting with ", model_name)
    preds <- predict(fit, test_data, type = "prob") %>%
      bind_cols(test_data) %>%
      mutate(.pred_class = as_factor(if_else(.pred_Yes > .pred_No, "Yes", "No")))
    
    # Check confusion matrix and metrics
    message("Confusion Matrix for ", model_name)
    conf_mat <- conf_mat(preds, truth = online_teaching, estimate = .pred_class)
    print(conf_mat)
    
    message("Metrics for ", model_name)
    metrics <- preds %>%
      metrics(truth = online_teaching, estimate = .pred_class)
    print(metrics)
  }
}

# Create a data frame of the metrics
metrics_df <- data.frame(
  Model = rep(c("Random Forest", "XGBoost"), each = 2),
  Metric = rep(c("Accuracy", "Kappa"), 2),
  Estimate = c(0.701, 0.0313, 0.701, 0.0313)
)

# Generate a simple table with kable
kable(metrics_df, "simple", booktabs = TRUE, caption = "Model Performance Metrics")

# Splitting the data
set.seed(123)
split <- initial_split(survey, prop = 0.8, strata = gender)
train_data <- training(split)
test_data <- testing(split)

#print("Training and Test data structures:")
#print(str(train_data))
#print(str(test_data))

# Model preparation
rf_spec <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("classification")

xgb_spec <- boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# Workflow creation and fitting models
workflow_list <- list(rf_spec, xgb_spec)
names(workflow_list) <- c("Random Forest", "XGBoost")
fits <- lapply(workflow_list, function(spec) {
  workflow() %>%
    add_formula(online_teaching ~ gender) %>%
    add_model(spec) %>%
    fit(data = train_data)
})

# Print fitted models
#lapply(fits, print)

# Predictions and metrics
for(model_name in names(fits)) {
  preds <- predict(fits[[model_name]], test_data, type = "prob") %>%
    bind_cols(test_data) %>%
    mutate(.pred_class = as_factor(if_else(.pred_Yes > .pred_No, "Yes", "No")))
  
  # Confusion matrix
  conf_mat <- conf_mat(preds, truth = online_teaching, estimate = .pred_class)
  #print(paste(model_name, "Confusion Matrix:"))
  #print(conf_mat)
  
  # Metrics
  metrics <- preds %>%
    metrics(truth = online_teaching, estimate = .pred_class)
  #print(paste(model_name, "Metrics:"))
  #print(metrics)
}

# Define the data frame
xgb_performance <- data.frame(
  iter = 1:10,  # Replace with the actual number of iterations you have
  training_logloss = c(0.6241134, 0.5892745, 0.5706919, 0.5608741, 
                       0.5554729, 0.5526112, 0.5511099, 0.5503296, 
                       0.5499500, 0.5497339)  # Replace with the actual log loss values you have
)

# Generate the table
xgb_performance %>%
  kable("simple", booktabs = TRUE, caption = "XGBoost Training Log Loss")

# Plot the training log loss over iterations for the XGBoost model
ggplot(xgb_performance, aes(x = iter, y = training_logloss)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "XGBoost Model Training Log Loss Over Iterations",
       x = "Iteration",
       y = "Training Log Loss") +
  theme_minimal()

metrics_rf <- data.frame(
  Model = "Random Forest",
  Metric = c("Accuracy", "Kappa"),
  Estimate = c(0.701, 0.0313)
)

metrics_xgb <- data.frame(
  Model = "XGBoost",
  Metric = c("Accuracy", "Kappa"),
  Estimate = c(0.701, 0.0313)
)

combined_metrics <- rbind(metrics_rf, metrics_xgb)

# Combine the metrics data frames for Random Forest and XGBoost
combined_metrics <- rbind(metrics_rf, metrics_xgb)

# Use kable to create a simple table with the combined metrics
kable(combined_metrics, "simple", booktabs = TRUE, caption = "Model Performance Metrics")

# Calculate net sentiment
sentiment_summary <- sentiment_summary %>%
  mutate(net_sentiment = positive - negative)

# Print net sentiment
#print(sentiment_summary)

# Assuming sentiment_summary is your data frame
sentiment_summary %>%
  kable("simple", booktabs = TRUE, caption = "Net Sentiment Summary")

# Print net sentiment value
#print(sentiment_summary$net_sentiment)

# Count the frequency of each word within each sentiment category
word_freq <- sentiment_scores %>%
  count(word, sentiment) %>%
  ungroup() %>%
  arrange(desc(n))

# Print the word frequencies to check
#print(head(word_freq))

# Create the plot with words frequency
#ggplot(word_freq, aes(x = word, y = n, color = sentiment)) +
#  geom_jitter(size = 2.5, width = 0.15) +
#  labs(x = "Word", y = "Frequency", color = "Sentiment") +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  ggtitle("Frequency of Words by Sentiment")

# Specify the number of top words you want to display
top_n_words <- 30

# Select the top N most frequent words
word_freq_top_n <- word_freq %>%
  arrange(desc(n)) %>%
  slice_max(order_by = n, n = top_n_words)

# Now plot only the top N words to avoid clutter
ggplot(word_freq_top_n, aes(x = reorder(word, n), y = n, color = sentiment)) +
  geom_jitter(size = 2.5, width = 0.15) +
  labs(x = "Word", y = "Frequency", color = "Sentiment") +
  theme(axis.text.x = element_text(angle = 65, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = "top") +
  ggtitle("Top Words by Sentiment Frequency") +
  coord_flip()  # Flipping coordinates for better readability
