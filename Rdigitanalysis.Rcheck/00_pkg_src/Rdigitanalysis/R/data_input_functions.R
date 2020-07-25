############################################################
#Functions for digit analysis R package
###Data input functions in this file
#Wenjun Chang
#Summer 2020
############################################################


############################################################
#Class function
############################################################


#create our own class
# Can extend and create with D(...) or new("D", ...)
#' @export DigitAnalysis
#' @exportClass DigitAnalysis
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

# #rounding function based on user preference
# round_data = function(data, method='round'){
#   if (method == 'round'){
#     return(round(data))
#   } else if (method == 'floor'){
#     return(floor(data))
#   } else if (method == 'ceiling'){
#     return(ceiling(data))
#   }
# }

#find the length of the largest number in a data column
max_length = function(data){
  max = max(floor(log10(data[!(is.na(data))]))+1)
  # max = 0
  # for (i in 1:length(data)){
  #   if (!(is.na(data[i]))) {
  #     if (floor(log10(data[i]))+1 > max){
  #       max = floor(log10(data[i]))+1
  #     }
  #   }
  # }
  return(max)
}

#drop rows with NaNs or empty strings in any numeric data column
# drop_nan_empty = function(df, col_conerned){
#   output = data.frame(df)
#   output = complete.cases(output[col_conerned]) #?? not sure if this work
#   # for (i in 1:length(col_conerned)){
#   #   output = output[!(is.na(output[[col_conerned[i]]]) | output[[col_conerned[i]]]==""), ]
#   # }
#   return(output)
# }

#align the digits from the left/right of a data column and update it to the specified data frame
align_digits = function(indata, outdata, naming_method, colname, align_direction='left'){

  max = max_length(indata) #max length of largest number in indata

  #intialize all digit places to NA
  for (i in 1:max){
    outdata[[paste(colname, naming_method[i])]] = NA
  }

  for (j in 1:length(indata)){
    #split each number into chars
    if (!(is.na(indata[j]))){
      chars = strsplit(as.character(indata[j]), "")[[1]]

      #reverse it since we are aligning from the right so right-first digit comes first
      if (align_direction == 'right'){
        chars = rev(chars)
      }

      for (k in 1:length(chars)){
        outdata[[paste(colname, naming_method[k])]][j] = as.integer(chars[k])
      }
    }
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
  # cleaned_data = drop_nan_empty(cleaned_data, col_analyzing)
  for (i in 1:length(col_analyzing)) {
    #name of current data column modifying
    col_name = col_analyzing[i]

    #turn into numbers
    cleaned_data[[col_name]] = as.numeric(gsub(",","",as.character(cleaned_data[[col_name]]), fixed=TRUE))

    #rounding
    cleaned_data[[col_name]] = as.integer(cleaned_data[[col_name]])
    #cleaned_data[[col_name]] = round_data(cleaned_data[[col_name]])
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

    indata = cleaned_data[[col_name]]

    #replace 0 by NA for digit analysis
    indata[indata == 0] = NA

    aligned_data = align_digits(indata=indata, outdata=aligned_data, naming_method=naming_method, colname=col_name, align_direction=align_direction) ###NAs introduced by coercion
  }
  return(aligned_data)
}

#make_aligned_data(DigitData@cleaned, 'ALEXP', DigitData@right_aligned_column_names, align_direction='right')

############################################################
#Data input main function
############################################################

###parse and clean the data for digit analysis
###col_analyzing can be specified as any of numeric_data='sdfsfsf' or numeric_data=c('dsdfsfsf') or numeric_data=c('dsdfsfsf',...)
###option to use delimeters with ',' as default
#filetype can be csv or excel
make_class = function(filepath, col_analyzing, delim=',', filetype='csv'){
  #data format: row is observation; column is category; must be csv!!!
  #raw input data->'raw' of the class
  raw_data = NA

  print('0')
  if (filetype == 'csv'){
    raw_data = read.csv(filepath, sep=delim, stringsAsFactors=FALSE)
  }
  else if (filetype == 'excel'){
    raw_data = readxl::read_excel(filepath) #really bad...takes 10 mins to load
    raw_data = data.frame(raw_data) #turn to dataframe..before is some weird type called tibble
  }
  else {
    stop('input file must be either csv or excel (.xls or .xlsx) file')
  }
  print('1')
  #remove all unnecessary blank columns
  raw_data = raw_data[colSums(!is.na(raw_data)) > 0]
  print('2')
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
  #cleaned_data = cleaned_data[which(complete.cases(cleaned_data['ALEXP.Values'])), ] #### for now
  print('3')
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

