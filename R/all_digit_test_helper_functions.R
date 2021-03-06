############################################################
# DigitAnalysis R Package
# github.com/jlederluis/digitanalysis
# Jetson Leder-Luis and Jean Ensminger
# Research assistant: Wenjun Chang
# Helper functions for digit test functions in this files
############################################################

############################################################################################################
#the following functions help parse data into desirable data forms ready for analysis
############################################################################################################

#' Grab all data column names when data_columns = 'all'
#'
#' @inheritParams all_digits_test
#'
#' @return \code{data_columns} as a numeric array
get_data_columns = function(digitdata, data_columns){
  #if 'all', use all data columns in the numeric columns specified by user
  if (length(data_columns) == 1){
    if (data_columns[1] == 'all'){
      data_columns = colnames(digitdata@numbers)
    }
  }
  return(data_columns)
}


#' Fetches the left/right aligned table for a single numeric data column
#'
#' @param desired_col The desired numeric data column: must be a string
#' @param align_direction 'left' or 'right': specify whether left-aligned or right-aligned digits table to be returned
#' @inheritParams all_digits_test
#'
#' @return The left-aligned or right-aligned digits table desired
#' @export
#'
#' @examples
#' single_column_aligned(digitdata, 'col_name1', align_direction='left')
#' single_column_aligned(digitdata, 'col_name2', align_direction='right')
single_column_aligned = function(digitdata, desired_col, align_direction='left'){

  original_df = NA
  if (align_direction == 'left') {
    original_df = digitdata@left_aligned
  }
  else if (align_direction == 'right') {
    original_df = digitdata@right_aligned
  }
  else {
    stop("align_direction must be either 'left' or 'right'")
  }

  #create output table
  single_align_df = data.frame(matrix(ncol = 0, nrow = length(original_df[,1])))
  column_names = colnames(original_df)
  for (i in 1:length(column_names)){
    #add a space at the end to avoid picking up alternative superstring column names or can be a '.' to separate the name
    if (grepl(paste(desired_col, ''), column_names[i], fixed=TRUE) || grepl(paste(desired_col, '.', sep=''), column_names[i], fixed=TRUE)){
      single_align_df[[column_names[i]]] = NA
      single_align_df[[column_names[i]]] = original_df[[column_names[i]]]
    }
  }
  #remove all NA columns
  single_align_df = single_align_df[colSums(!is.na(single_align_df)) > 0]
  return(single_align_df)
}


#' Remove last digit from a left-aligned/right-aligned digits table
#'
#' @param single_column_digits left-aligned/right-aligned digits table for a single numeric column
#' @param align_direction 'left' or 'right': specify whether left-aligned or right-aligned digits table \code{single_column_digits} is
#'
#' @return \code{single_column_digits} without last digits
drop_last_digit_places = function(single_column_digits, align_direction='left'){
  #left aligned
  if (align_direction == 'left'){
    rows = dim(single_column_digits)[1]
    cols = dim(single_column_digits)[2]
    for (row in 1:rows){
      #find the first column that is NA, which is the last digit + 1
      index = 1
      while ((index <= cols) && (!(is.na(single_column_digits[row, index])))){ #########might have potential bug here... & vs. &&
        index = index + 1
      }
      if (index <= cols + 1) {
        #index - 1 is the last digit; turn that cell to NA
        single_column_digits[row, index-1] = NA
      }
    }
  }
  #right aligned
  else if (align_direction == 'right'){
    column_names = colnames(single_column_digits)
    #drop all columns for 1s
    for (i in 1:length(column_names)){
      if (grepl(' 1s' , column_names[i], fixed=TRUE)){
        single_column_digits = single_column_digits[!(colnames(single_column_digits) %in% c(column_names[i]))]
      }
    }
  }
  else{
    stop("align_direction must be either 'left' or 'right'")
  }
  return(single_column_digits)
}


#' Remove first digit from a left-aligned/right-aligned digits table
#'
#' @inheritParams drop_last_digit_places
#'
#' @return \code{single_column_digits} without first digits
drop_first_digit_places = function(single_column_digits, align_direction='left'){

  #left aligned
  if (align_direction == 'left'){
    column_names = colnames(single_column_digits)
    #drop all columns for 1st digit
    for (i in 1:length(column_names)){
      if (grepl('1st digit' , column_names[i], fixed=TRUE)){
        single_column_digits = single_column_digits[!(colnames(single_column_digits) %in% c(column_names[i]))]
      }
    }
  }
  #right aligned
  else if (align_direction == 'right'){
    rows = dim(single_column_digits)[1]
    cols = dim(single_column_digits)[2]
    for (row in 1:rows){
      #find the first column that is not NA, which is the first digit
      index = 1
      while ((index <= cols) && (is.na(single_column_digits[row, index]))){ #########might have potential bug here... & vs. &&
        index = index + 1
      }
      #turn that cell to NA
      single_column_digits[row, index] = NA
    }
  }
  else{
    stop("align_direction must be either 'left' or 'right'")
  }
  return(single_column_digits)
}


#' Fetches the left-aligned/right-aligned data columns for analysis, drop the first and last digit places if desired
#'
#' @param digit_places Only used internally for checking if it is last digit test
#' @inheritParams single_column_aligned
#' @inheritParams all_digits_test
#'
#' @return A list with
#' \itemize{
#'   \item \code{digits_table} The left-aligned/right-aligned digits table for \code{data_columns}
#'   \item \code{digitdata} The \code{DigitAnalysis} object with slot 'max' updated
#' }
grab_desired_aligned_columns = function(digitdata, data_columns, skip_first_digit=FALSE, skip_last_digit=FALSE, align_direction='left', digit_places=0){
  #last digit test
  if (digit_places[1] == -1){
    digits_table = data.frame(matrix(ncol = 0, nrow = length(digitdata@numbers[,1])))
    #handle the data_columns = 'all' situation
    data_columns = get_data_columns(digitdata, data_columns)
    #digits_table-->usable data
    for (col_name in data_columns){
      single_column_digits = single_column_aligned(digitdata, col_name, 'right') #get rihght aligned 1s column
      digits_table = cbind(digits_table, single_column_digits[ncol(single_column_digits)])
    }
    digitdata@max = 1
    return(list('digits_table'=digits_table, 'digitdata'=digitdata))
  }

  digits_table = data.frame(matrix(ncol = 0, nrow = length(digitdata@numbers[,1])))

  #handle the data_columns = 'all' situation
  data_columns = get_data_columns(digitdata, data_columns)

  digitdata@max = 0
  for (col_name in data_columns){
    single_column_digits = single_column_aligned(digitdata, col_name, align_direction)

    #update the max attribute of digitdata for use in other functions
    if (digitdata@max < as.numeric(length(single_column_digits))){
      digitdata@max = as.numeric(length(single_column_digits))
    }
    #remove last digit before remove first, to avoid problems like there are 1-digit numbers!!!!!!!!
    if (skip_last_digit){
      single_column_digits = drop_last_digit_places(single_column_digits, align_direction)
    }
    if (skip_first_digit){
      single_column_digits = drop_first_digit_places(single_column_digits, align_direction)
    }
    digits_table = cbind(digits_table, single_column_digits) ###### can turn this into a better function using the library cannoot recall now, use row bind
  }
  return(list('digits_table'=digits_table, 'digitdata'=digitdata))
}


#' Obtain exclusively the desired digit places for left aligned digits table
#'
#' @param digits_table A left-aligned digits table
#' @inheritParams all_digits_test
#'
#' @return Digits table with desired digit places, ready to count observation
parse_digit_places = function(digitdata, digits_table, digit_places){
  #last digit test...no need to parse
  if (digit_places[1] == -1){
    return(digits_table)
  }
  #find the names of the digit places to drop
  digit_places_names = digitdata@left_aligned_column_names[-digit_places]

  #create a copy of the table to be returned
  usable_data = data.frame(digits_table)
  colnames(usable_data) = gsub("."," ",colnames(usable_data), fixed=TRUE)

  #drop by scanning each column name
  if (!(is.na(digit_places_names[1]))){
    for (position_name in digit_places_names){
      for (i in 1:length(colnames(digits_table))){
        if (grepl(position_name, colnames(digits_table)[i], fixed=TRUE)){
          #drop this column since it is the digit place unwanted
          usable_data = usable_data[!(colnames(usable_data) %in% c(colnames(digits_table[i])))]
        }
      }
    }
  }
  return(usable_data)
}


#' Parse cleaned digits table from \code{parse_digit_places} to obtain observation table for chi square test
#'
#' @param usable_data Digits table with desired digit places
#' @inheritParams all_digits_test
#'
#' @return Observation table for chi square test
obtain_observation = function(digitdata, usable_data, digit_places, skip_first_digit, skip_last_digit, omit_05){
  #last digit test
  if (digit_places[1] == -1){
    #create a table for collecting observations for n=max digit places, upper bound
    observation_table = data.frame(matrix(0, nrow=10, ncol=1))
    #name the rows
    rownames(observation_table) = 0:9
    for (i in 1:length(usable_data)){
      occurances = table(usable_data[i], useNA = 'no')
      #update it to column j of observation table
      #this occurances can be a null table
      if (!(is.null(occurances))){
        for (name in names(occurances)){
          digit = as.integer(name)
          #digit + 1 since index starts from 1 and digit starts from 0
          observation_table[digit+1, 1] = observation_table[digit+1, 1] + occurances[name] #name = str(digit)    ######can simplify
        }
      }
    }
    #omit 05
    if (!is.na(omit_05[1])){
      observation_table = data.frame(observation_table[!rownames(observation_table) %in% as.character(omit_05), ])
      rownames(observation_table) = as.character(0:9)[!0:9 %in% omit_05]
    }
    #name the columns
    colnames(observation_table) = 'Last Digit'
    return(observation_table)
  }

  #create a table for collecting observations for n=max digit places, upper bound
  observation_table = data.frame(matrix(0, nrow=10, ncol=digitdata@max))

  #fill up observation table from usable data columns
  digit_place_names = digitdata@left_aligned_column_names
  #name the columns
  colnames(observation_table) = digit_place_names[1:length(observation_table)]
  #name the rows
  rownames(observation_table) = 0:9

  for (i in 1:length(usable_data)){
    #figure out the digit place it is in
    for (j in 1:length(observation_table)){
      if (grepl(paste('',digit_place_names[j]), colnames(usable_data)[i], fixed=TRUE)){
        #it is a column for digit place j
        #get the table for frequency count for column i
        occurances = table(usable_data[,i], useNA = 'no')
        #update it to column j of observation table
        #this occurances can be a null table
        if (!(is.null(occurances))){
          for (name in names(occurances)){
            digit = as.integer(name)
            #digit + 1 since index starts from 1 and digit starts from 0
            observation_table[digit+1, j] = observation_table[digit+1, j] + occurances[name] #name = str(digit)    ######can simplify
          }
        }
      }
    }
  }
  if (length(omit_05) == 2){
    #drop both 0 and 5
    observation_table = observation_table[-c(1,6), ]
  }
  else if (length(omit_05) == 1){
    #drop 0
    if (!(is.na(omit_05))){
      observation_table = observation_table[-c(1), ]
    }
    #otherwise, omit_05 is NA so do nothing
  }
  #get the digit places we are interested in
  observation_table = observation_table[digit_places]

  #drop first digit col
  if (skip_first_digit){
    observation_table = observation_table[!(colnames(observation_table) %in% c('1st Digit'))]
  }
  return(observation_table)
}

#' Parse the contingency table to have exclusively the desired digits and digit places
#'
#' @inheritParams all_digits_test
#'
#' @return Contingency table with exclusively the desired digits and digit places
parse_contingency_table = function(digitdata, contingency_table, digit_places, skip_first_digit, skip_last_digit, omit_05){
  #last digit test
  if (digit_places[1] == -1){
    last_digit_expected = digitanalysis::uniform_table[1]
    if (!is.na(omit_05[1])){
      last_digit_expected = data.frame(last_digit_expected[!rownames(last_digit_expected) %in% as.character(omit_05), ])
      rownames(last_digit_expected) = as.character(0:9)[!0:9 %in% omit_05]
    }
    colnames(last_digit_expected) = 'Last Digit Place'
    return(last_digit_expected)
  }
  #drop the "x" and Digits column for table
  contingency_table = contingency_table[!(colnames(contingency_table) %in% c("Digits", "X"))]

  if (length(omit_05) == 2){
    #drop both 0 and 5
    contingency_table = contingency_table[-c(1,6), ]
  }
  else if (length(omit_05) == 1){
    #drop 0
    if (!(is.na(omit_05))){
      contingency_table = contingency_table[-c(1), ]
    }
    #otherwise, omit_05 is NA so do nothing
  }

  #find the digit places to use and drop the extra digit places in precomputred table
  contingency_table = contingency_table[digit_places]
  #####more checkings....
  if (skip_first_digit){
    contingency_table = contingency_table[!(colnames(contingency_table) %in% c('Digit Place 1'))]
  }
  #renormalize each column to sum to 1
  for (name in colnames(contingency_table)){
    #renormialize
    contingency_table[name] = contingency_table[name] / sum(contingency_table[name])
  }
  return(contingency_table)
}


############################################################################################################
#additional funnctions satisfying advanced user options for the analysis
############################################################################################################


#' Breaks the data on specified column
#'
#' @param data A dataframe, preferably 'cleaned' slot for \code{DigitAnalysis} object any parsed version of it
#' @inheritParams all_digits_test
#'
#' @return A list of array of indexes in \code{data} that belongs to each category
break_by_category = function(data, break_out, break_out_grouping){

  if (is.na(match(break_out, colnames(data)))) {
    stop('specified category is not a column in the data')
  }
  if (typeof(data[, break_out]) != "character"){
    stop('specified break out column is not a category column')
  }
  indexes_of_categories = list()
  for (category_name in unique(data[, break_out])){
    #what if there is NA? havent encountered yet...I guess ignore

    #get the rows for each broken-down category
    rows = which(data[[break_out]] == category_name)

    #add rows to the named element in list
    indexes_of_categories[[category_name]] = rows
  }

  if (!(is.na(break_out_grouping[1]))){
    #merge the indexes for categories in the same grouping to same list element
    indexes_of_grouping = list()
    for (group_name in names(break_out_grouping)){
      #get the index of sector by accessing the names of the categories in the data column that belong to this sector
      indexes_of_group = indexes_of_categories[break_out_grouping[[group_name]]]
      indexes_of_grouping[[group_name]] = unlist(indexes_of_group) #turn into an array and update output list
    }
    return(indexes_of_grouping)
  }
  return(indexes_of_categories)
}


#' Create a sub-object of the \code{digitdata} object given the indexes of the rows that belong to the sub-object.
#'
#' @param indexes The indexes of the rows that belong to the sub-object.
#' @inheritParams all_digits_test
#'
#' @return A sub-object of input \code{digitdata}.
#' @export
make_sub_digitdata = function(digitdata, indexes){
  sub_digitdata = digitdata
  #raw is a 'round' or 'unround' for unpack rounded test
  if (length(sub_digitdata@raw) != 1){
    sub_digitdata@raw = data.frame(sub_digitdata@raw[indexes, ])
    colnames(sub_digitdata@raw) = colnames(digitdata@raw)
  }
  sub_digitdata@cleaned = data.frame(sub_digitdata@cleaned[indexes, ])
  colnames(sub_digitdata@cleaned) = colnames(digitdata@cleaned)
  sub_digitdata@numbers = data.frame(sub_digitdata@numbers[indexes, ])
  colnames(sub_digitdata@numbers) = colnames(digitdata@numbers) #in case numbers is a single column
  sub_digitdata@left_aligned = data.frame(sub_digitdata@left_aligned[indexes, ])
  colnames(sub_digitdata@left_aligned) = colnames(digitdata@left_aligned)
  sub_digitdata@right_aligned = data.frame(sub_digitdata@right_aligned[indexes, ])
  colnames(sub_digitdata@right_aligned) = colnames(digitdata@right_aligned)
  return(sub_digitdata)
}

#' Display p-value as decimals when p-value >= 0.001, and as scientific when p-value < 0.001.
#'
#' @param p_value The p-value computed
#'
#' @return p-value with better formatting
format_p_values = function(p_value){
  if (is.na(p_value)){
    return(p_value)
  }
  p_value = format(p_value, digits = 4)
  if (!is.na(p_value) && p_value < 0.001){
    p_value = format(p_value, scientific = TRUE, digits = 4)
  }
  return(p_value)
}
