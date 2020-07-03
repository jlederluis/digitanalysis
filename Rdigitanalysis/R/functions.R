############################################################
#Functions for digit analysis R package
###Data input functions in this file
#Wenjun Chang
#Summer 2020
############################################################

#############prelim############
#clear workspace
rm(list = ls())
#free up R memory
gc()
#force numerical representation rather than scientific
options(scipen = 999)
##############################


############################################################
#Class function
############################################################


#create our own class
DigitAnalysis = setClass('DigitAnalysis', slots = c(raw="data.frame", cleaned="data.frame",
                                                    numbers="data.frame", left_aligned="data.frame",
                                                    right_aligned="data.frame", left_aligned_column_names='character',
                                                    right_aligned_column_names='character', max='numeric'))


############################################################
#basic helper functions for data input function
############################################################


#this thing blow up my storage several times for some reason??????
#library("readxl")
#xlxs = read_excel('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.xlsx')

#rounding function based on user preference
round_data = function(data, method='round'){
  if (method == 'round'){
    return(round(data))
  } else if (method == 'floor'){
    return(floor(data))
  } else if (method == 'ceiling'){
    return(ceiling(data))
  }
}

#find the length of the largest number in a data column
max_length = function(data){
  max = 0
  for (i in 1:length(data)){
    if (!(is.na(data[i]))) {
      if (floor(log10(data[i]))+1 > max){
        max = floor(log10(data[i]))+1
      }
    }
  }
  return(max)
}

#drop rows with NaNs or empty strings in any numeric data column
drop_nan_empty = function(df, col_conerned){
  output = data.frame(df)
  for (i in 1:length(col_conerned)){
    output = output[!(is.na(output[[col_conerned[i]]]) | output[[col_conerned[i]]]==""), ]
  }
  return(output)
}

#align the digits from the left/right of a data column and update it to the specified data frame
align_digits = function(indata, outdata, naming_method, align_direction='left', colname='Unknown'){

  max = max_length(indata) #max length of largest number in indata
  #intialize all digit places to NA
  for (i in 1:max){
    outdata[[paste(colname, naming_method[i])]] = NA
  }

  for (j in 1:length(indata)){
    #split each number into chars
    chars = strsplit(as.character(indata[j]), "")[[1]]

    #reverse it since we are aligning from the right so right-first digit comes first
    if (align_direction == 'right'){
      chars = rev(chars)
    }

    for (k in 1:length(chars)){
      outdata[[paste(colname, naming_method[k])]][j] = as.integer(chars[k])
    }

    # #reverse the dataframe back if we are aligning from the right for better visual
    # if (align_direction == 'right'){
    #   outdata = rev(outdata)
    # }
  }
  return(outdata)
}


############################################################
#intermediate helper functions for data input function
############################################################


#clean up the number columns with numeric data to be analyzed->'cleaned' of the class
make_cleaned_data = function(raw_data, col_analyzing){
  cleaned_data = data.frame(raw_data) #make copy without pointer issue

  #drop rows with NaNs or empty strings in any numeric data column
  cleaned_data = drop_nan_empty(cleaned_data, col_analyzing)

  for (i in 1:length(col_analyzing)) {
    #name of current data column modifying
    col_name = col_analyzing[i]

    #turn into numbers
    cleaned_data[[col_name]] = as.numeric(gsub(",","",cleaned_data[[col_name]], fixed=TRUE))

    #rounding
    cleaned_data[[col_analyzing[i]]] = round_data(cleaned_data[[col_name]])
  }

  return(cleaned_data)
}

#the dataframe with only the data being analyzed->'numeric' of the class
make_numeric_data = function(cleaned_data, col_analyzing){

  #initialization
  numeric_data = data.frame(matrix(ncol = 0, nrow = length(cleaned_data[,1])))

  for (i in 1:length(col_analyzing)) {
    #name of current data column modifying
    col_name = col_analyzing[i]

    #update 'numeric' object
    numeric_data[[col_name]] = NA
    numeric_data[[col_name]] = cleaned_data[[col_name]]
  }

  return(numeric_data)
}

#the dataframe with the left/right aligned digits of each data column to be analyzed->'left_aligned'/'right_aligned' of the class
make_aligned_data = function(cleaned_data, col_analyzing, naming_method, align_direction='left'){

  #initialization
  aligned_data = data.frame(matrix(ncol = 0, nrow = length(cleaned_data[,1])))

  for (i in 1:length(col_analyzing)) {
    #name of current data column modifying
    col_name = col_analyzing[i]

    #update by 'align_left' or 'align_right'
    aligned_data = align_digits(indata=cleaned_data[[col_name]], outdata=aligned_data, naming_method=naming_method, align_direction=align_direction, colname=col_name)
  }

  return(aligned_data)
}



############################################################
#Data input main function
############################################################

###parse and clean the data for digit analysis
###col_analyzing can be specified as any of numeric_data='sdfsfsf' or numeric_data=c('dsdfsfsf') or numeric_data=c('dsdfsfsf',...)
###option to use delimeters with ',' as default
make_class = function(filepath, col_analyzing, delim=','){
  #data format: row is observation; column is category; must be csv!!!
  #raw input data->'raw' of the class
  raw_data = read.csv(filepath, sep=delim)

  ####################################
  ############important###############
  ####################################
  #hard-coded way of naming the digit places, should be sufficient, if not can further add
  left_aligned_column_names = c('1st digit', '2nd digit', '3rd digit', '4th digit', '5th digit', '6th digit', '7th digit',
                                '8th digit', '9th digit', '10th digit', '11th digit', '12th digit', '13th digit')
  right_aligned_column_names = c('1s', '10s', '100s', '1k', '10k', '100k', '1m', '10m', '100m', '1b', '10b', '100b', '1t')
  ####################################
  ############important###############
  ####################################

  ########################creation of all sub-objects########################

  cleaned_data = make_cleaned_data(raw_data, col_analyzing)

  numeric_data = make_numeric_data(cleaned_data, col_analyzing)

  #the dataframe with the left aligned digits of each data column to be analyzed->'left_aligned' of the class
  #i.e. a column is 'X'; first digit will be at column '1st digit' + 'X'
  left_aligned_data = make_aligned_data(cleaned_data, col_analyzing, naming_method=left_aligned_column_names, align_direction='left')

  #the dataframe with the right aligned digits of each data column to be analyzed->'right_aligned' of the class
  #i.e. a column is 'X'; first digit will be at column '1s' + 'X'
  right_aligned_data = make_aligned_data(cleaned_data, col_analyzing, naming_method=right_aligned_column_names, align_direction='right')
  #reverse the dataframe for better visual in normal form
  right_aligned_data = rev(right_aligned_data)

  ########################creation of DigitAnalysis class object########################

  DigitData = DigitAnalysis(raw=raw_data, cleaned=cleaned_data, numbers=numeric_data, left_aligned=left_aligned_data, right_aligned=right_aligned_data,
                            left_aligned_column_names=left_aligned_column_names, right_aligned_column_names=right_aligned_column_names, max=0)

  return(DigitData)
}



###########################################################################################################################################################################################
#Digit test functions
###########################################################################################################################################################################################

############################################################
#basic helper functions for digit test functions
############################################################

#generate contingency table for Benford distribution (expected digit frequency under Benford's Law)
#for n digit places, and store it in a file for later use (as an option)
#In the contingency table, columnn are digit place in increasing order,
#whereas row is the digit (0 to 9) in increasing order
#gc() free up R memory cuz it might blow up
#N <= 6 takes roughly no time, but N > 6 are suspetible
#N = 7 takes 5 mins
#N = 8 takes 30 mins
Benford_table = function(N, out_fp, save=TRUE){

  contingency_table = data.frame(Digits=0:9)
  for (n in 1:N){
    if (n == 1){
      #first digit we hard code
      current_freqs = rep(0, 10)
      for (digit in 1:9){
        current_freqs[digit+1] = log10(1+1/digit)
      }
      #update table
      contingency_table[[paste('Digit Place', as.character(n))]] = NA
      contingency_table[[paste('Digit Place', as.character(n))]] = current_freqs
    }
    else {
      #digits place greater than the first
      args = list(1:9) #first digit has no 0
      args = append(args, rep(list(0:9), n-2))
      #create all possible combinations of the prefix digits
      n_digits_combinations = expand.grid(args)
      n_digits_combinations = apply(n_digits_combinations, 1, paste, collapse='')

      current_freqs = rep(NA, 10)
      for (digit in 0:9){
        freq = 0
        for (prefix in n_digits_combinations){
          denom = as.integer(paste(prefix,as.character(digit),sep=''))
          freq = freq + log10(1+1/as.integer(denom))
        }
        current_freqs[digit+1] = freq
      }
      #update table
      contingency_table[[paste('Digit Place', as.character(n))]] = NA
      contingency_table[[paste('Digit Place', as.character(n))]] = current_freqs
    }
  }
  #drop the weird "X" column in df #############Guess it does not work, reload again it appears
  contingency_table = contingency_table[ , !(colnames(contingency_table) %in% c("X"))]

  #save file if specified
  if (save) {
    write.csv(contingency_table, out_fp)
  }
  return(contingency_table)
}

#
#Benford_table(N=8, out_fp='C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\contingency_table.csv')

#load Benford table given filepath
load_Benford_table = function(fp){
  contingency_table = read.csv(fp)
  #get rid of '.' replacing ' ' problem when loading csv to df
  colnames(contingency_table) = gsub("."," ",colnames(contingency_table), fixed=TRUE)
  return(contingency_table)
}


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


#a=single_column_aligned(DigitData, "ALEXP", 'left')
#head(a,5)

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
        print(column_names[i])
        #drops = c(column_names[i])
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
      print('sfdsff')
      print(head(single_column_digits))
      print(as.numeric(length(single_column_digits)))
      print(digitdata@max)
      digitdata@max = as.numeric(length(single_column_digits))
      print(digitdata@max)
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
        usable_data = usable_data[ , !(colnames(usable_data) %in% c(colnames(digits_table[i])))]
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

  for (i in 1:length(usable_data)){
    #figure out the digit place it is in
    for (j in 1:length(observation_table)){
      if (grepl(paste('',digit_place_names[j]), colnames(usable_data)[i], fixed=TRUE)){
        #it is a column for digit place j
        #get the table for frequency count for column i
        occurances = table(usable_data[,i])
        #update it to column j of observation table
        print(occurances)
        for (name in names(occurances)){
          digit = as.integer(name)
          print(observation_table[digit+1, j] + occurances[name])
          #digit + 1 since index starts from 1 and digit starts from 0
          observation_table[digit+1 , j] = observation_table[digit+1, j] + occurances[name] #name = str(digit)
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
    observation_table = observation_table[ , -digit_places]
  }
  if (look_or_omit == 'look'){
    observation_table = observation_table[ , digit_places]
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
    contingency_table = contingency_table[ , -digit_places]
  }
  else if (look_or_omit == 'look'){
    contingency_table = contingency_table[ , digit_places]
  }

  #####more checkings....
  if (skip_first_figit){
    contingency_table = contingency_table[ , !(colnames(contingency_table) %in% c('Digit Place 1'))]
  }
  return(contingency_table)
}


############################################################
#all digits test
############################################################

################main function############
#performs all-digit place two-way chi square test vs Benford’s Law
#DigitData is the class object;
#data_columns are the names of numerical columns of data to be analyzed (defaulted as 'all' to the entire number table)
#digit_places are the indexes of digit places desired to test on
###can be single digit for single digit test, or an array of digits for multiple digit test
###-1 can only appear alone as for last digit test
###e.g. digit_places = 'all', 5, -1, c(1,2,3), etc.
#look_or_omit can be either 'look' or 'omit', to either omit or look exclusively at the digit_places specified
#omit_05 has three options: omit both 0 and 5->c(0,5); omit only 0->0 or c(0); and omit neither->NA (when no rounding test is performed)
#if analysis by groups is desired, break_out should specify the deisred category to break upon
#distribution can be 'Benford' or 'Uniform' or more ???
#if last_digit_test_included is true, will omit last digit before analysis, since we don't want tests to overlap
#unpacking_rounding_column specifies the data column (has to be numeric!) to split upon rounded
#and unrounded numbers to perform unpacking rounding test


#!!!last_digit_test_included should overwrite digit_places and (skip_first_digits)
all_digits_test = function(digitdata, contingency_table, data_columns='all', digit_places='all', look_or_omit='look',
                           skip_first_figit=TRUE, omit_05=c(0,5), break_out=NA, distribution='Benford', plot=TRUE,
                           last_digit_test_included=FALSE, unpacking_rounding_column=NA){

  #############some logical stuff to check and throw errors on###########
  #this must be multiple digits test
  if ((digit_places != 'all') && (length(digit_places) > 1 )){
    #thus should not have -1 as part of the array
    if (!is.na(match(-1, digit_places))){
      stop('multiple digits test cannot have last digit as part of the digit places')
    }
  }

  if ((look_or_omit == 'look') && (skip_first_figit == TRUE)){
    #should not have 1 as part of the array since we are skipping first digit place
    if (!is.na(match(1, digit_places))){
      stop('look_or_omit and skip_first_figit contradicts, both looking and not looking at the first digit place')
    }
  }

  if (length(omit_05) == 1){
    ###check omit only 5, which is not allowed
    if (!(is.na(omit_05)) && (omit_05 == 5)){
      stop('cannot omit only 5 without also omitting 0 first')
    }
  }

  if (length(digit_places) == 1){
    #thus should not have -1 as part of the array
    if (last_digit_test_included){
      stop('not using any data')
    }
    if (skip_first_figit){
      if (digit_places == 1){
        stop('not using any data')
      }
    }
  }

  #######################################################################
  #parse the data
  #######################################################################

  align_direction = 'left'

  #get the digits of the desired data columns to be analyzed
  lst = grab_desired_aligned_columns(digitdata, data_columns, skip_first_figit, last_digit_test_included, align_direction)
  digitdata = lst$digitdata
  digits_table = lst$digits_table

  #get only the wanted digit places
  usable_data = parse_digit_places(digitdata, digits_table, digit_places, look_or_omit)

  #parse only needed parts of contingency table
  contingency_table = parse_contigency_table(digitdata, contingency_table, digit_places, look_or_omit, skip_first_figit, last_digit_test_included, omit_05)

  #get observation table from usable data
  observation_table = obtain_observation(digitdata, usable_data, look_or_omit, skip_first_figit, last_digit_test_included, omit_05)



  #######################################################################
  #do chi square test
  #######################################################################

  #helper
  ##break on category
  ##break on round unround


  Xsq=chisq.test(as.matrix(observation_table), p = as.matrix(contingency_table))

  print(observation_table)
  print(contingency_table)
  return(Xsq)

  #this chisquare test has some problem
}

#############################################################
#############################################################
#tesiting
#############################################################



# #############try it with given data
#
# #load data input functions
# data_columns = c("ALEXP","BENTOT")#, "BENM", "BENF")
# fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv'
#
# DigitData = make_class(filepath = fp, col_analyzing = data_columns)
# #head(DigitData@right_aligned)
#
# #############################################################
# #############################################################
# #############################################################
#
# align_direction = 'left'
# skip_first_figit=FALSE
# last_digit_test_included=FALSE
# lst = grab_desired_aligned_columns(DigitData, data_columns, skip_first_figit, last_digit_test_included, align_direction)
# DigitData = lst$digitdata
# digits_table = lst$digits_table
# head(digits_table)
#
# digit_places = c(1,2,3)
# look_or_omit = 'look'
# DigitData@max
#
# usable_data = parse_digit_places(DigitData, digits_table, digit_places, look_or_omit)
# head(usable_data)
#
# #
# #load Benford table
# contingency_table = read.csv('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\contingency_table.csv')
# #get rid of '.' replacing ' ' problem when loading csv to df
# colnames(contingency_table) = gsub("."," ",colnames(contingency_table), fixed=TRUE)
#
#
#
# contingency_table
#
#
# #parse omit digits
# omit_05 = c(0,5)
# #length of observed table
# look_or_omit = 'look'
# digit_places = c(1,2,3)
#
# contingency_table=parse_contigency_table(DigitData, contingency_table, digit_places, look_or_omit, skip_first_figit, last_digit_test_included, omit_05)
# contingency_table
#
# observation_table = obtain_observation(DigitData, usable_data, look_or_omit, skip_first_figit, last_digit_test_included)
# observation_table
#
#
#
#
# Xsq=chisq.test(as.matrix(observation_table), p = as.matrix(contingency_table))
# Xsq$observed   # observed counts (same as M)
# Xsq$expected   # expected counts under the null
# Xsq$residuals  # Pearson residuals
# Xsq$stdres     # standardized residuals



#############################################################
#############################################################
#############################################################
#############try it with given data

#load data input functions
data_columns = c("ALEXP","BENTOT", "BENM", "BENF")
fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv'

DigitData = make_class(filepath = fp, col_analyzing = data_columns)
#head(DigitData@right_aligned)

contingency_table = load_Benford_table('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\contingency_table.csv')
contingency_table

# #
# #load Benford table
# contingency_table = read.csv('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\contingency_table.csv')
# #get rid of '.' replacing ' ' problem when loading csv to df
# colnames(contingency_table) = gsub("."," ",colnames(contingency_table), fixed=TRUE)
# contingency_table

digit_places = c(1,2,3)
look_or_omit = 'look'
skip_first_figit=FALSE
omit_05 = NA#c(0,5)
# break_out=NA
# distribution='Benford'
# plot=TRUE
last_digit_test_included=FALSE
# unpacking_rounding_column=NA

Xsq=all_digits_test(digitdata = DigitData, contingency_table = contingency_table, data_columns = data_columns, digit_places = digit_places, look_or_omit = look_or_omit,
                           skip_first_figit = skip_first_figit, omit_05 = omit_05, break_out=NA, distribution='Benford', plot=TRUE,
                           last_digit_test_included=FALSE, unpacking_rounding_column=NA)

Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals







#helper
##break on category
clean = DigitData@cleaned

head(clean[, clean[["DIST"]] == unique(clean$DIST)[1]][1:10])



#helper
##split round and unround










######some garbage

head(DigitData@raw,2)
head(DigitData@cleaned,2)
head(DigitData@numbers,2)
a=head(DigitData@left_aligned,2)
b=head(DigitData@right_aligned,2)

test = DigitAnalysis(cleaned = data.frame(matrix(ncol = 1, nrow = 1)), values=data.frame(matrix(ncol = 1, nrow = 1)))
test
slotNames(class(DigitAnalysis))

test@values = data.frame(x=1)
test
data.frame(1)
slot(test, "cleaned")
test@values

x = 123.55
trunc(x)
floor(x)
ceiling(x)

round(x, digits = 0)

######

