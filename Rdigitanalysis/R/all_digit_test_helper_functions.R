############################################################
#Functions for digit analysis R package
###helper functions for digit test functions in this file
#Wenjun Chang
#Summer 2020
############################################################


############################################################################################################
#the following funnctions help parse data into desirable data forms ready for analysis
############################################################################################################


#gives the table for the left/right aligned table for a single numeric data column ############################
#digitdata is an object of the class DigitAnalysis!                               ############################ Also user friendly, can let user use it to check their data
#desired_col should be a string!
single_column_aligned = function(digitdata, desired_col, align_diretion='left') {
  if (is.na(match(desired_col, colnames(digitdata@numbers)))){
    #throw error
    stop("Specified desired_col is not a numerical data column in the specified DigitAnalysis class object")
  } else {
    #add a space at the end to avoid picking up alternative superstring column names
    checking = paste(desired_col, '')
    original_df = NA

    if (align_diretion == 'left') {
      original_df = DigitData@left_aligned
    }
    else if (align_diretion == 'right') {
      original_df = DigitData@right_aligned
    }
    else {
      stop("align_direction must be either 'left' or 'right'")
    }

    #create output table
    single_align_df = data.frame(matrix(ncol = 0, nrow = length(original_df[,1])))
    column_names = colnames(original_df)

    for (i in 1:length(column_names)){
      if (grepl(checking, column_names[i], fixed=TRUE)){
        single_align_df[[column_names[i]]] = NA
        single_align_df[[column_names[i]]] = original_df[[column_names[i]]]
      }
    }

    return(single_align_df)
  }
}


#remove last digit if desired
drop_last_digit_places = function(digitdata, single_column_digits, align_direction='left'){
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
        single_column_digits = single_column_digits[ , !(colnames(single_column_digits) %in% c(column_names[i]))]
      }
    }
  }
  return(single_column_digits)
}


#remove first digit if desired
drop_first_digit_places = function(digitdata, single_column_digits, align_direction='left'){

  #left aligned
  if (align_direction == 'left'){
    column_names = colnames(single_column_digits)
    #drop all columns for 1st digit
    for (i in 1:length(column_names)){
      if (grepl('1st digit' , column_names[i], fixed=TRUE)){
        single_column_digits = single_column_digits[ , !(colnames(single_column_digits) %in% c(column_names[i]))]
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
  return(single_column_digits)
}

#gets the desired data columns for analysis, can be left or right aligned
#also in the mean time drop the first and last digit places if it is desired
grab_desired_aligned_columns = function(digitdata, data_columns, skip_first_figit=TRUE, last_digit_test_included=FALSE, align_direction='left'){

  digits_table = data.frame(matrix(ncol = 0, nrow = length(digitdata@numbers[,1])))
  for (col_name in data_columns){
    single_column_digits = single_column_aligned(digitdata, col_name, align_direction)

    #update the max attribute of digitdata for use in other functions
    if (digitdata@max < as.numeric(length(single_column_digits))){
      #print('sfdsff')
      #print(head(single_column_digits))
      #print(as.numeric(length(single_column_digits)))
      #print(digitdata@max)
      digitdata@max = as.numeric(length(single_column_digits))
      #print(digitdata@max)
    }

    ###!!!!!!!!!!!!!!!!!!!!!!!
    #remove last digit before remove first, to avoid problems like there are 1-digit numbers
    if (last_digit_test_included){
      single_column_digits = drop_last_digit_places(digitdata, single_column_digits, align_direction)
    }
    if (skip_first_figit){
      single_column_digits = drop_first_digit_places(digitdata, single_column_digits, align_direction)
    }

    digits_table = cbind(digits_table, single_column_digits)
  }
  return(list('digits_table'=digits_table, 'digitdata'=digitdata))
}

#on desired aligned columns, extract only the desired digit places in a dropping column based way
#only applies for left aligned digits data!!!!!!!!!!
parse_digit_places = function(digitdata, digits_table, digit_places, look_or_omit){

  #find the names of the digit places to drop
  if (look_or_omit == 'omit'){
    digit_places_names = digitdata@left_aligned_column_names[digit_places]
  }
  if (look_or_omit == 'look'){
    digit_places_names = digitdata@left_aligned_column_names[-digit_places]
  }

  #create a copy of the table to be returned
  usable_data = data.frame(digits_table)
  colnames(usable_data) = gsub("."," ",colnames(usable_data), fixed=TRUE)


  #drop by scanning each column name
  for (position_name in digit_places_names){
    for (i in 1:length(colnames(digits_table))){
      if (grepl(position_name, colnames(digits_table)[i], fixed=TRUE)){
        #drop this column since it is the digit place unwanted
        usable_data = usable_data[!(colnames(usable_data) %in% c(colnames(digits_table[i])))]
      }
    }
  }
  return(usable_data)
}


#parse usable_data to  obtain observation table s.t. we have exclusively the desired digits and digit places
obtain_observation = function(digitdata, usable_data, look_or_omit, skip_first_figit, last_digit_test_included, omit_05){
  #create a table for collecting observations for n=max digit places
  observation_table = NA
  if (last_digit_test_included){
    observation_table = data.frame(matrix(0, nrow=10, ncol=digitdata@max-1)) #one less digit place
  } else {
    observation_table = data.frame(matrix(0, nrow=10, ncol=digitdata@max))
  }
  #fill up observation table from usable data columns
  digit_place_names = digitdata@left_aligned_column_names
  #name the columns
  colnames(observation_table) = digit_place_names[1:length(observation_table)]
  #name the rows
  rownames(observation_table) = 0:9

  for (i in 1:length(usable_data)){
    #figure out the digit place it is in
    for (j in 1:length(observation_table)){
      #print(usable_data)
      #print(paste('',digit_place_names[j]))
      #print(colnames(usable_data)[i])
      #print(grepl(paste('',digit_place_names[j]), colnames(usable_data)[i], fixed=TRUE))

      if (grepl(paste('',digit_place_names[j]), colnames(usable_data)[i], fixed=TRUE)){
        #it is a column for digit place j
        #get the table for frequency count for column i
        occurances = table(usable_data[,i])
        #update it to column j of observation table
        #print(occurances)
        #this occurances can be a null table
        if (!(is.null(occurances))){
          for (name in names(occurances)){
            digit = as.integer(name)
            #print(observation_table[digit+1, j] + occurances[name])
            #digit + 1 since index starts from 1 and digit starts from 0
            observation_table[digit+1 , j] = observation_table[digit+1, j] + occurances[name] #name = str(digit)
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

  #find the names of the digit places to drop
  if (look_or_omit == 'omit'){
    observation_table = observation_table[-digit_places]
  }
  if (look_or_omit == 'look'){
    observation_table = observation_table[digit_places]
  }
  #drop first digit col
  if (skip_first_figit){
    observation_table = observation_table[ , !(colnames(observation_table) %in% c('1st Digit'))]
  }
  return(observation_table)
}

#parse the contigency table s.t. we have exclusively the desired digits and digit places
parse_contigency_table = function(digitdata, contingency_table, digit_places, look_or_omit, skip_first_figit, last_digit_test_included, omit_05){
  #drop the "x" and Digits column for table
  contingency_table = contingency_table[ , !(colnames(contingency_table) %in% c("Digits", "X"))]

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

  #drop the extra digit places in precomputred table

  #####checkings
  end = digitdata@max
  if (last_digit_test_included){
    end = end - 1
  }
  contingency_table = contingency_table[ , 1:end]
  #####more checkings

  #find the names of the digit places to drop/use
  if (look_or_omit == 'omit'){
    contingency_table = contingency_table[-digit_places]
  }
  else if (look_or_omit == 'look'){
    contingency_table = contingency_table[digit_places]
  }

  #####more checkings....
  if (skip_first_figit){
    contingency_table = contingency_table[ , !(colnames(contingency_table) %in% c('Digit Place 1'))]
  }
  return(contingency_table)
}


############################################################################################################
#additional funnctions satisfying advanced user options for the analysis
############################################################################################################


#split on category and perform chi square test on data for each category
#data is a data frame, usually the cleaned df or any parsed version of it from digitdata class object
break_by_category = function(data, break_out){

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
    rows = which(data[[break_out]] %in% category_name)

    #add rows to the named element in list
    indexes_of_categories[[category_name]] = rows
  }

  return(indexes_of_categories)
}

#split round and unround numbers on specified column to perform unpacking round number test
unpacking_round_number_split = function(digitdata, unpacking_rounding_column){
  if (is.na(match(unpacking_rounding_column, colnames(digitdata@cleaned)))) {
    stop('specified category is not a column in the data')
  }
  if (typeof(digitdata@cleaned[[unpacking_rounding_column]]) == "character"){
    stop('the column for splitting unround and round numbers must be a column with numbers')
  }

  rounded_rows = which(digitdata@cleaned[[unpacking_rounding_column]] %% 10 == 0)

  return(rounded_rows)
}

