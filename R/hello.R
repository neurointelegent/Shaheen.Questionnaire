# library(tensorflow)
# install_tensorflow(method = "conda")
#
# library(keras)
# install_keras(method = "conda")
#


# load libraries

load_libraries <- function(){

  pacman::p_load(tidyverse, readxl, tableone, knitr,
                 kableExtra, factoextra,  finalfit,
                 caret,mice, VIM, naniar, dbscan)

}



# load libraries
load_analysis <- function(){

  pacman::p_load(tidyverse, readxl, tableone, knitr, kableExtra, factoextra,  finalfit, caret)

}





# Functions


# Replace rare categorizes with "Other"

replace_rare_levels <- function(factor, threshold, newLevel) {
  # Get the frequency table of the factor levels
  freq_table <- table(factor)
  # Get the names of the levels that have frequency less than or equal to the threshold
  rare_levels <- names(freq_table)[freq_table <= threshold]
  # Replace the rare levels with "other" in the factor
  factor <- as.character(factor)
  factor[factor %in% rare_levels] <- newLevel
  factor <- as.factor(factor)
  # Return the modified factor
  return(factor)
}

# Make nice table
mykable <- function(x, caption = NA, row.names = FALSE, ...){
  kable(x, row.names = row.names, align = c("l", "l", "r", "r", "r", "r", "r", "r", "r"),
        booktabs = TRUE, caption = caption,
        linesep = c("", "", "\\addlinespace"), ...) %>%
    kable_styling(latex_options = c("scale_down", "hold_position"))
}

# Define a function that takes a data frame, a column name, and a character to replace
replace_char <- function(df, col, char) {
  # Get the unique values of the column
  values <- unique(df[[col]])
  # Count the frequency of each value
  freq <- table(df[[col]])
  # Find the most frequent value
  mode <- names(freq)[which.max(freq)]
  # Replace the character with the most frequent value
  df[[col]] <- as.character(df[[col]])
  df[[col]][df[[col]] == char] <- mode
  df[[col]] <- as.factor(df[[col]])
  # Return the modified data frame
  return(df)
}

# select a column name depending on threshold frequency
list_cols <- function(df, char, freq) {
  # Initialize an empty list to store the column names
  col_list <- list()
  # Loop through each column of the data frame
  for (col in names(df)) {
    # Count the number of times the character appears in the column
    count <- sum(grepl(char, df[[col]]))
    # If the count is equal to the frequency, add the column name to the list
    if (count >= freq) {
      col_list <- c(col_list, col)
    }
  }
  # Return the list of column names
  return(col_list)
}



# Define a function that takes a vector of strings and a part of string as arguments
remove_string <- function(vec, part) {
  # Use the str_remove_all function from the stringr package to remove the part from each string
  library(stringr)
  vec <- vec[!str_detect(vec, part)]
  # Return the modified vector
  return(vec)
}



# Removes a column names depending on a threshould frequency
remove_cols_by_freq <- function(data, freq) {
  # loop through each column of the data frame
  for (col in names(data)) {
    # check if the column is a factor
    if (is.factor(data[[col]])) {
      # get the frequency table of the column
      tab <- table(data[[col]])
      # check if any category has a frequency equal or greater than the specified value
      if (any(tab <= freq)) {
        # remove the column from the data frame
        data <- data[,!(names(data) %in% col)]
      }
    }
  }
  # return the modified data frame
  return(data)
}





# Define a function to replace characters in all columns
replace_characters <- function(data, mapping) {
  # Check if the data is a data frame
  if (!is.data.frame(data)) {
    stop("The data must be a data frame")
  }
  # Check if the mapping is a named vector
  if (!is.vector(mapping) || !is.character(mapping) || is.null(names(mapping))) {
    stop("The mapping must be a named character vector")
  }
  # Loop through the columns of the data
  for (col in names(data)) {
    # Check if the column is character or factor
    if (is.character(data[[col]]) || is.factor(data[[col]])) {
      # Replace the characters according to the mapping
      data[[col]] <- as.character(data[[col]])
      data[[col]] <- mapping[data[[col]]]
    }
  }
  # Return the modified data
  return(data)
}



# Define a function to assign symptom column
assign_symptom <- function(data, freq_col, sev_col) {
  # Check if the data is a data frame
  if (!is.data.frame(data)) {
    stop("The data must be a data frame")
  }
  # Check if the frequency and severity columns exist
  if (!freq_col %in% names(data)) {
    stop("The frequency column does not exist in the data")
  }
  if (!sev_col %in% names(data)) {
    stop("The severity column does not exist in the data")
  }
  # Check if the frequency and severity columns are numeric
  if (!is.numeric(data[[freq_col]])) {
    stop("The frequency column must be numeric")
  }
  if (!is.numeric(data[[sev_col]])) {
    stop("The severity column must be numeric")
  }
  # Apply the 2-2 threshold to assign symptom
  symptom <- ifelse(data[[freq_col]] >= 2 & data[[sev_col]] >= 2, "Yes", "No")
  # Return the modified data
  return(symptom)
}




# Define a function to make a new data frame of sum of the columns
make_new_data_frame <- function(data, col_list) {
  # Check if the data is a data frame
  if (!is.data.frame(data)) {
    stop("The data must be a data frame")
  }
  # Check if the col_list is a named list of numeric vectors
  if (!is.list(col_list) || !all(sapply(col_list, is.numeric)) || is.null(names(col_list))) {
    stop("The col_list must be a named list of numeric vectors")
  }
  # Initialize an empty data frame
  new_data_frame <- data.frame(rowNames=1:nrow(data))

  # Loop through the names of the col_list
  for (name in names(col_list)) {
    # Get the column indices for the name
    col_indices <- col_list[[name]]
    # Check if the column indices are valid
    if (any(col_indices < 1) || any(col_indices > ncol(data))) {
      stop("The column indices must be within the range of the data")
    }
    # Sum the columns for the name
    new_data_frame[[name]] <- rowSums(data[, c(col_indices)])
  }
  # Return the new data frame
  return(new_data_frame)
}



# Make a new variable from a list of variables with levels of (0,1) with class of factor
make_new_variable <- function(df, cols) {
  # Check if the input is a data frame
  if (!is.data.frame(df)) {
    stop("The input must be a data frame")
  }
  # Check if the vector of column names is valid
  if (!is.vector(cols) || !all(cols %in% names(df))) {
    stop("The vector of column names must be valid and exist in the data frame")
  }
  # Create a vector that is 1 if any of the columns in the vector are 1, and 0 otherwise
  new_var <- as.integer(rowSums(df[cols] == 1) > 0)
  # Return the vector
  return(as.factor(new_var))
}



