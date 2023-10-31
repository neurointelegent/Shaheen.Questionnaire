\name{Shaheen.Questionnaire}
\alias{Shaheen.Questionnaire}
\title{Shaheen.Questionnaire}
\usage{
Shaheen.Questionnaire::function()
}
\description{
# Make a new variable from a list of variables with levels of (0,1) with class of factor
make_new_variable(df, cols)

# Define a function to make a new data frame of sum of the columns
make_new_data_frame(data, col_list)

# Define a function to assign symptom column
assign_symptom(data, freq_col, sev_col) # Return the modified data

# Define a function to replace characters in all columns
replace_characters(data, mapping)

# Removes a column names depending on a threshould frequency
remove_cols_by_freq(data, freq)

# Define a function that takes a vector of strings and a part of string as arguments
remove_string(vec, part)

# select a column name depending on threshold frequency
list_cols(df, char, freq)

# Define a function that takes a data frame, a column name, and a character to replace
replace_char(df, col, char)

# Make nice table
mykable(x, caption = NA, row.names = FALSE, ...)


# Replace rare categorizes with "Other"
replace_rare_levels(factor, threshold, newLevel)

}
\examples{

}
