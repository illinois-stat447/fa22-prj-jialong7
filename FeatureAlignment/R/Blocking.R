#' Helper Function, Find Unmatchable features' index
#'
#' @param dt a datatable
#'
#' @return a vector of index
#'
FindUnmatchable = function(dt) {
  right_most = length(dt)
  result = c()
  for (i in 1:right_most) { # for each feature
    unique_values = unique(dt[,i,with = FALSE]) # unique values
    value = dt[[1,i]]
    n_of_rows = length(dt)
    idx = 1
    while (is.na(value) & idx < n_of_rows) { # jump NA
      idx = idx + 1
      value = dt[[idx,i]]
    }
    # 3 types of invalid data -> 0, NA, ""
    if ((nrow(unique_values) == 1) & (is.na(value) | value == 0 | stri_isempty(value))) {
      result = append(result, i)
    }
  }
  return(result)
}

#' Helper function, Find time features' index
#'
#' @param dt a datatable
#'
#' @return a vector of index
#'
FindTime = function(dt) {
  right_most = length(dt)
  result = c()
  for (i in 1:right_most) { # for each feature
    first_value = dt[[1,i]]
    idx = 1
    n_of_rows = nrow(dt)
    while (is.na(first_value) & idx < n_of_rows) { # jump NA
      idx = idx + 1
      first_value = dt[[idx,i]]
    }
    # "" can not be detected if we use "is.na"
    flag = tryCatch(
      error = function(cnd) FALSE,
      as.Date(first_value)
    )
    # class(first_value) == "character" is useful when ""
    if (class(flag) == "Date" & !is.na(first_value) & !stri_isempty(first_value)) {
      result = append(result, i)
    }
  }
  return(result)
}

#' Helper function, Find Integer features' index
#'
#' @param dt a data table
#'
#' @return a vector of index
#'
FindInteger = function(dt) {
  right_most = length(dt)
  result = c()
  for (i in 1:right_most) { # for each feature
    value = dt[[1,i]]
    idx = 1
    n_of_rows = nrow(dt)
    while (is.na(value) & idx < n_of_rows) { # jump NA
      idx = idx + 1
      value = dt[[idx,i]]
    }
    if (typeof(value) == "integer") {
      result = append(result, i)
    }
  }
  return(result)
}

#' Helper function, Find Double features' index
#'
#' @param dt a data table
#'
#' @return a vector of index
#'
FindDouble = function(dt) {
  right_most = length(dt)
  result = c()
  for (i in 1:right_most) { # for each feature
    value = dt[[1,i]]
    idx = 1
    n_of_rows = nrow(dt)
    while (is.na(value) & idx < n_of_rows) { # jump NA
      idx = idx + 1
      value = dt[[idx,i]]
    }
    if (typeof(value) == "double") {
      result = append(result, i)
    }
  }
  return(result)
}

#' Helper function, Find String features' index
#'
#' @param dt a data table
#'
#' @return a vector of index
#'
FindString = function(dt) {
  right_most = length(dt)
  result = c()
  for (i in 1:right_most) { # for each feature
    value = dt[[1,i]]
    idx = 1
    n_of_rows = nrow(dt)
    while (is.na(value) & idx < n_of_rows) { # jump NA
      idx = idx + 1
      value = dt[[idx,i]]
    }
    if (typeof(value) == "character") {
      result = append(result, i)
    }
  }
  return(result)
}

#' BaseBlock
#'
#' @param dt a data.table
#' @param type Unmatchable, Time, String, Int, Double, ALL.
#'
#' @return a vector of index
#' @export
#'
BaseBlock = function(dt, type = "All") {
  # Return Unmatchable
  Unmatchable = FindUnmatchable(dt)
  if (type == "Unmatchable") return(Unmatchable)

  # Return Time
  Time_dirty = FindTime(dt) # remove unmatchable
  Time = Time_dirty[!Time_dirty %in% Unmatchable]
  if (type == "Time") return(Time)

  # Return String
  # remove unmatchable + time
  Interated_1 = c(Unmatchable,Time)
  String_dirty = FindString(dt)
  String = String_dirty[!String_dirty %in% Interated_1]
  if (type == "String") return(String)

  # Return Int
  # remove unmatchable + time + string
  Interated_2 = c(Interated_1,String)
  Int_dirty = FindInteger(dt)
  Int = Int_dirty[!Int_dirty %in% Interated_2]
  if (type == "Int") return(Int)

  # Return Double
  # remove unmatchable + time + string + Int
  Interated_3 = c(Interated_2,Int)
  Double_dirty = FindDouble(dt)
  Double = Double_dirty[!Double_dirty %in% Interated_3]
  if (type == "Double") return(Double)

  if (type == "All") {
    # return a df
    cat("Unmatchable Indexes:", "\n", Unmatchable, "\n")
    cat("Time Indexes:", "\n", Time, "\n")
    cat("String Indexes:", "\n", String, "\n")
    cat("Int Indexes:", "\n", Int, "\n")
    cat("Double Indexes:", "\n", Double, "\n")
  }
}

#' AdvancedBlock
#'
#' @param dt a datatable
#' @param type Binary, Categorical, Neither.
#'
#' @return a vector of index
#' @export
#'
AdvancedBlock = function(dt, type) {
  binary <- c()
  categorical_not_binary <- c()
  Neither <- c()
  Int = BaseBlock(dt,"Int")
  Double = BaseBlock(dt,"Double")
  String = BaseBlock(dt,"String")

  whole_features = c(Int, Double, String)
  selected_index = sort(whole_features)

  for (i in seq_along(selected_index)) {
    idx = selected_index[i]
    feature_temp = as.data.frame(dt[,..idx])
    feature_temp_1 <- replace(feature_temp, feature_temp =='', NA)
    feature = drop_na(feature_temp_1) # feature drop NA
    varies = nrow(unique(feature))
    # consider t, f, NULL
    if (type == "Binary" & varies == 2) {
      binary = append(binary, selected_index[i])
    }
    threshold = nrow(dt) / 20
    if (type == "Categorical not Binary" & varies > 2 & varies <= threshold) {
      categorical_not_binary = append(categorical_not_binary, selected_index[i])
    }
    if (type == "Neither" & varies > threshold) {
      Neither = append(Neither, selected_index[i])
    }
  }
  Neither_droped = Neither[!Neither %in% String]
  if (type == "Binary") {
    return(binary)
  }
  if (type == "Categorical not Binary") {
    return(categorical_not_binary)
  }
  if (type == "Neither") {
    return(Neither_droped)
  }
}


#' This function visualizes similarity comparison between numerical features.
#'
#' @param feature1 a column of data
#' @param feature2 a column of data
#' @param input_sample_size how many data point to include
#'
#' @return a map
#' @export
#'
visual_inspect_returns_ks_test_result = function(feature1, feature2, input_sample_size = 100) {
  get_numeric_dataframe = function(x, center_data = FALSE) {
    # get rid of anything that's not digit or "." or "-", because numerical values could have "." or negative signs.
    x = gsub("[^[:digit:], ^., ^-]", "", x)
    # get rid of ","
    x = gsub("[,]", "", x)
    x = as.numeric(x)
    if (center_data) {
      x = x - mean(x, na.rm = TRUE)
    }
    x = as_tibble(x)
    return(x)
  }
  max_sample_size = min(length(feature1), length(feature2))
  actual_sample_size = min(input_sample_size, max_sample_size)
  feature1_sampled = sample(feature1, actual_sample_size)
  feature2_sampled = sample(feature2, actual_sample_size)
  col1 = get_numeric_dataframe(feature1_sampled)
  names(col1) = "feature1"
  col2 = get_numeric_dataframe(feature2_sampled)
  names(col2) = "feature2"
  df = data.frame(first_feature = col1, second_feature = col2)
  x_label = paste("Sampled", as.character(actual_sample_size), "values")
  gg1 = ggplot(data = df) +
    geom_density(mapping = aes(feature1), color = "blue") +
    geom_density(mapping = aes(feature2), color = "red") +
    labs(x = x_label, y = "Density",
         title = "Density Comparison",
         subtitle = ("The closer they are the more likely they are the same feature")
    )
  show(gg1)
  # large value in ks test indicates they're (probably) the same feature
  ks_test_result = suppressWarnings(ks.test(df$feature1, df$feature2))
  return(ks_test_result)
}


#' This function returns a mapping: a vector of length = length(dt1)
#'
#' @param dt1 a data table
#' @param dt2 a data table
#' @param input_sample_size sample how many data points
#' @param type type = "Int" or "Double"
#'
#' @return a vector of matching result
#' @export
#'
find_best_match_based_on_KS_test = function(dt1, dt2, input_sample_size = 500, type) {

  # This helper function compare one feature from the first data_table to all the features in the second data_table; and calculate the p-value from ks.test. It returns the index of the maximum p-value.
  # Usage: type = "Int" or "Double"

  find_best_index = function(current_feature, numeric_indexes, input_sample_size) {
    # numeric_indexes is the indexes of the numerical columns of the second data_table
    # initialize a "ks_results" vector to store the indexes
    ks_results = numeric(length(numeric_indexes))
    # sample size should not be greater than length(current_feature)
    sample_size1 = min(input_sample_size, length(current_feature))
    for(i in numeric_indexes){
      feature1_sampled = sample(current_feature, sample_size1)
      # sample size should not be greater than length(dt2[[i]]))
      sample_size2 = min(input_sample_size, length(dt2[[i]]))
      feature2_sampled = sample(dt2[[i]], sample_size2)
      ks_results[i] = ks.test(feature1_sampled, feature2_sampled)$p.value
    }
    return(which.max(ks_results))
  }
  numeric_indexes_dt1 = BaseBlock(dt1, type = type)
  numeric_indexes_dt2 = BaseBlock(dt2, type = type)
  # create a vector to store the mapping
  mapping = numeric(length(dt1))
  for(i in numeric_indexes_dt1) {
    mapping[i] = find_best_index(current_feature = dt1[[i]], numeric_indexes = numeric_indexes_dt2, input_sample_size)
  }
  return(mapping)
}

#' get_occurrence_num returns the number of occurrences of each character in characters_to_match for char_feature
#'
#' @param char_feature a character vector
#' @param characters_to_match a character vector
#' @param input_sample_size a numeric vector of length 1
#'
#' @return a numeric vector recording the number of occurrence of each character in characters_to_match
#' @export
#'
get_occurrence_num = function(char_feature, characters_to_match = letters, input_sample_size = 500) {
  actual_sample_size = min(input_sample_size, length(char_feature))
  occurrence_vec = numeric(length(characters_to_match))
  char_feature_sampled = as.character(sample(char_feature, actual_sample_size))
  for (i in seq_along(characters_to_match)) {
    occurrence_vec[i] = sum(str_count(string = char_feature_sampled, pattern =
                                        characters_to_match[i]))
  }
  return(occurrence_vec)
}

#' returns a data frame containing the number of occurrences of each character in characters_to_match for every character columns in the input data table
#'
#' @param dt a data table
#' @param characters_to_match characters to match
#' @param input_sample_size a numeric vector of length 1; sample to reduce computational time
#'
#' @return data frame
#' @export
#'
get_occurrence_matrix_as_df = function(dt, characters_to_match = letters, input_sample_size = 500) {
  char_indexes = FindString(dt)
  occurrence_matrix_df = as.tibble(matrix(nrow = length(characters_to_match), ncol = length(char_indexes)))
  for (i in seq_along(char_indexes)) {
    temp_index = char_indexes[[i]]
    to_append = get_occurrence_num(dt[[temp_index]], characters_to_match = characters_to_match, input_sample_size = input_sample_size)
    occurrence_matrix_df[i] = to_append
  }
  #colnames(occurrence_matrix_df) = names(char_indexes)
  return(occurrence_matrix_df)
}

#' This function returns the matching result, which is of the same length of dt1
#'
#' @param dt1 a data table
#' @param dt2 a data table
#' @param characters_to_match characters to match
#' @param sample_size a numeric vector of length 1; use sample size to reduce computational time
#'
#' @return a numeric vector of the same length(dt1)
#' @export
#'
find_best_match_character_features = function(dt1, dt2, characters_to_match = letters, sample_size = 500) {
  char_indexes1 = FindString(dt1)
  char_indexes2 = FindString(dt2)
  occrurrence_matrix1 = get_occurrence_matrix_as_df(dt = dt1, characters_to_match = characters_to_match)
  occrurrence_matrix2 = get_occurrence_matrix_as_df(dt = dt2, characters_to_match = characters_to_match)
  cosine_values = numeric(length = length(dt2))
  mapping = numeric(length(dt1))
  for (j in seq_along(char_indexes1)) {
    for (i in seq_along(char_indexes2)) {
      cosine_values[i] = calculate_cosine(occrurrence_matrix1[[j]], occrurrence_matrix2[[i]])
    }
    mapping[char_indexes1[j]] = char_indexes2[which.max(cosine_values)]
  }
  return(mapping)
}

#' calculate the consine of vec1 and vec 2
#'
#' @param vec1 a numeric vector
#' @param vec2 a numeric vector
#'
#' @return a numeric vector of lengh 1; the value is between 0 and 1
#' @export
#'
calculate_cosine = function(vec1, vec2) {
  return(cosine(x = vec1, y = vec2))
}

#' the ultimate function that combines the results of find_best_match_based_on_KS_test and find_best_match_character_features
#'
#' @param dt1 a data table
#' @param dt2 a data table
#' @param sample_size_numeric sample size to reduce runtime
#' @param sample_size_string sample size to reduce runtime
#' @param characters_to_match letters
#'
#' @return returns a numeric vector of length equal to length(dt1); c(3,0,1) means the first column in dt1 matched with the third column in dt2, the second column in dt1 couldn't be matched with any column in dt2, and the third column in dt1 is matched with the first column on dt2.
#' @export
#'
auto_matching = function(dt1, dt2, sample_size_numeric = 500, sample_size_string = 500, characters_to_match = letters) {
  matching_double = suppressWarnings(find_best_match_based_on_KS_test(dt1 = dt1, dt2 = dt2, type = "Double", input_sample_size = sample_size_numeric))
  matching_int = suppressWarnings(find_best_match_based_on_KS_test(dt1 = dt1, dt2 = dt2, type = "Int", input_sample_size = sample_size_numeric))
  matching_numeric = matching_double + matching_int
  matching_string = suppressWarnings(find_best_match_character_features(dt1 = dt1, dt2 = dt2, characters_to_match = characters_to_match))
  matching_all = matching_numeric + matching_string
  return(matching_all)
}

