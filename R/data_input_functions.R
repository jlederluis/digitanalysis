############################################################
# DigitAnalysis R Package
# github.com/jlederluis/digitanalysis
# Jetson Leder-Luis and Jean Ensminger
# Research assistant: Wenjun Chang
# Data input functions in this file
############################################################

############################################################
#Class function
############################################################

#' The class object for RDigitAnalysis package
#'
#' @slot raw The raw dataframe passed in with a filepath or a dataframe without without NA columns
#' @slot cleaned The cleaned dataframe with numeric data columns truncated and turned into integers if it is previous characters
#' @slot numbers The numeric columns analyzing
#' @slot left_aligned The left-aligned digits of each data column to be analyzed in \code{numbers}. i.e. a column is 'X'; first digit will be at column '1st digit' + 'X'.
#' @slot right_aligned The right-aligned digits of each data column to be analyzed in \code{numbers}. i.e. a column is 'X'; first digit will be at column '1s' + 'X'.
#' @slot left_aligned_column_names The naming method for left-aligned digit places in \code{left_aligned}
#' @slot right_aligned_column_names The naming method for right-aligned digit places in \code{right_aligned}
#' @slot max The maximum length of the numbers being analyzed in \code{all_digit_test}. Default to 0, not relevant outside of \code{all_digit_test}.
#'
#' @export DigitAnalysis
#' @exportClass DigitAnalysis
DigitAnalysis = setClass('DigitAnalysis', slots = c(raw="data.frame", cleaned="data.frame",
                                                    numbers="data.frame", left_aligned="data.frame",
                                                    right_aligned="data.frame", left_aligned_column_names='character',
                                                    right_aligned_column_names='character', max='numeric'))
#set some methods for DigitAnalysis only
setMethod(f = 'show',
          signature = 'DigitAnalysis',
          definition = function(object){
            print("An object of class 'DigitAnalysis'")
            print('Slots:')
            print(slotNames(object))
          })

#classof = function(x) {return("DigitAnalysis")}
#method.skeleton("class", "DigitAnalysis")
# setGeneric("class",
#            function(x) {
#              return("DigitAnalysis")
#            })
# setMethod(f = 'class',
#           signature = c("DigitAnalysis"),
#           definition = function(x) {
#             class = 'DigitAnalysis'
#             class
#           })


############################################################
#basic helper functions for data input function
############################################################

#' Find the length of the largest number in a data column
#'
#' @param data Data column analyzed
#'
#' @return Maximum length of the largest number in the data column
max_length = function(data){
  max = max(floor(log10(data[!(is.na(data))]))+1)
  return(max)
}


#' Align the digits from the left/right of a data column and update it to the specified data frame
#'
#' @param indata The data column to be turned into aligned digits dataframe
#' @param outdata The dataframe \code{indata} will be appended (cbind) to
#' @param naming_method Either \code{left_aligned_column_names} or \code{right_aligned_column_names} from \code{DigitAnalysis} class
#' @param col_name Column name for \code{indata}
#' @param align_direction 'left' or 'right': Create left-aligned or right-aligned digits
#'
#' @return \code{outdata}: outdata appended with indata's aligned digits
align_digits = function(indata, outdata, naming_method, col_name, align_direction='left'){
  max = max_length(indata) #max length of largest number in indata

  #intialize all digit places to NA
  for (i in 1:max){
    outdata[[paste(col_name, naming_method[i])]] = NA
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
        outdata[[paste(col_name, naming_method[k])]][j] = as.integer(chars[k])
      }
    }
  }
  return(outdata)
}

############################################################
#intermediate helper functions for data input function
############################################################

#' Load raw dataframe from file or from passed-in dataframe
#'
#' @inheritParams process_digit_data
#'
#' @return Raw dataframe for \code{DigitAnalysis} slot 'raw'
make_raw_data = function(filepath=NA, filetype='csv', delim=',', raw_df=NA){
  raw_data = NA
  #load with fp
  #data format: row is observation; column is category; must be csv or excel!!!
  if (!(is.na(filepath)) && is.na(raw_df)){
    if (filetype == 'csv'){
      raw_data = read.csv(filepath, sep=delim, stringsAsFactors=FALSE)
    }
    else if (filetype == 'excel'){
      print('Loading excel file...might be slow...')
      raw_data = readxl::read_excel(filepath) #really bad...takes 10 mins to load
      raw_data = data.frame(raw_data) #turn to dataframe..before is some weird type called tibble
    }
    else {
      stop('input file must be either csv or excel (.xls or .xlsx) file')
    }
  }
  #pass in a dataframe
  else if (!(is.na(raw_df)) && is.na(filepath)){
    if (!(is.data.frame(raw_df))){
      stop('"raw_df" must be a dataframe')
    }
    raw_data = raw_df
  }
  else {
    stop('Can either use filepath to load raw data or pass in a dataframe as raw data. Cannot do both or neither!')
  }

  #remove all unnecessary blank columns
  raw_data = raw_data[colSums(!is.na(raw_data)) > 0]
  return(raw_data)
}


#' Clean up the numeric columns of dataframe
#'
#' @param raw_data 'raw' slot for \code{DigitAnalysis}
#' @param digit_columns The numeric columns to be cleaned
#'
#' @return Cleaned dataframe for \code{DigitAnalysis} slot 'cleaned'
make_cleaned_data = function(raw_data, digit_columns){
  cleaned_data = data.frame(raw_data) #make copy without pointer issue

  for (i in 1:length(digit_columns)) {
    #name of current data column modifying
    col_name = digit_columns[i]

    #turn into numbers
    cleaned_data[[col_name]] = as.numeric(gsub(",","",as.character(cleaned_data[[col_name]]), fixed=TRUE))

    #rounding
    cleaned_data[[col_name]] = as.integer(cleaned_data[[col_name]])
  }
  #drop rows with all! NaNs in all! numeric data column
  cleaned_data = as.data.frame(cleaned_data[rowSums(is.na(cleaned_data[digit_columns])) != length(digit_columns), ])

  return(cleaned_data)
}


#' Create dataframe for only the numeric columns of the cleaned data
#'
#' @param cleaned_data 'cleaned' slot for \code{DigitAnalysis}
#' @param digit_columns The numeric columns to be used
#'
#' @return Numeric dataframe for \code{DigitAnalysis} slot 'numbers'
make_numeric_data = function(cleaned_data, digit_columns){
  numeric_data = data.frame(matrix(ncol = 0, nrow = length(cleaned_data[,1])))
  numeric_data[digit_columns] = cleaned_data[digit_columns]
  return(numeric_data)
}

#' Create the dataframe with the left/right aligned digits of each data column to be analyzed->'left_aligned'/'right_aligned' of the class
#'
#' @param cleaned_data 'cleaned' slot for DigitAnalysis
#' @param digit_columns The numeric columns to be used
#' @param naming_method Either \code{left_aligned_column_names} or \code{right_aligned_column_names} from \code{DigitAnalysis} class
#' @param align_direction 'left' or 'right': Create left-aligned or right-aligned dataframe
#'
#' @return The dataframe with the left/right aligned digits of each data column to be analyzed for \code{DigitAnalysis} slot 'left_aligned'/'right_aligned'
make_aligned_data = function(cleaned_data, digit_columns, naming_method, align_direction='left'){
  #initialize
  aligned_data = data.frame(matrix(ncol = 0, nrow = length(cleaned_data[,1])))

  for (i in 1:length(digit_columns)) {
    #name of current data column modifying
    col_name = digit_columns[i]

    #update by 'align_left' or 'align_right'
    indata = cleaned_data[[col_name]]

    #replace 0 by NA for digit analysis
    indata[indata == 0] = NA

    aligned_data = align_digits(indata=indata, outdata=aligned_data, naming_method=naming_method, col_name=col_name, align_direction=align_direction) ###NAs introduced by coercion
  }
  return(aligned_data)
}

############################################################
#Data input main function
############################################################

#' Create an object instance for \code{DigitAnalysis}. Parse and clean the data for digit analysis.
#'
#' @param filepath Default to NA. If loading data using filepath, specify filepath as a string.
#' @param digit_columns All potential datra columns to be analyzed. Defaulted to NA. Can be specified as any of
#' \itemize{
#'   \item \code{digit_columns} = 'col_name'
#'   \item \code{digit_columns} = c('col_name')
#'   \item \code{digit_columns} = c('col_name1','col_name2', ...)
#' }
#' @param delim Defaulted to ','. Can specify other delimeters as well.
#' @param filetype Default to 'csv'. If loading data using filepath, specify either 'csv' or 'excel'.
#' 'excel' option supports both 'xlsx' and 'xls' format.
#' @param raw_df Default to NA. If loading data using a dataframe. Pass in the dataframe instance.
#'
#' @return An object in \code{DigitAnalysis}
#' @export
#'
#' @examples
#' process_digit_data('col_name', filepath='~/filename.csv')
#' process_digit_data('col_name', filepath='~/filename.xlsx', filetype='excel', delim=',')
#' process_digit_data('col_name', raw_df=my_dataframe)
process_digit_data = function(filepath=NA, digit_columns=NA, filetype='csv', delim=',', raw_df=NA){

  ############important###############
  #hard-coded way of naming the digit places, should be sufficient, if not can further add
  left_aligned_column_names = c('1st digit', '2nd digit', '3rd digit', '4th digit', '5th digit', '6th digit', '7th digit',
                                '8th digit', '9th digit', '10th digit', '11th digit', '12th digit', '13th digit')
  right_aligned_column_names = c('1s', '10s', '100s', '1k', '10k', '100k', '1m', '10m', '100m', '1b', '10b', '100b', '1t')
  ############important###############

  if (any(is.na(digit_columns))){
    stop('digit_columns cannot be NA. Must analyze something!')
  }

  ########################creation of all sub-objects########################

  print('Reading from file...') #for user

  raw_data = make_raw_data(filepath, filetype, delim, raw_df)

  print('Cleaning data...') #for user

  cleaned_data = make_cleaned_data(raw_data, digit_columns)

  numeric_data = make_numeric_data(cleaned_data, digit_columns)

  print('Digitizing columns...') #for user

  left_aligned_data = make_aligned_data(cleaned_data, digit_columns, naming_method=left_aligned_column_names, align_direction='left')

  right_aligned_data = make_aligned_data(cleaned_data, digit_columns, naming_method=right_aligned_column_names, align_direction='right')
  #reverse the dataframe for better visual in normal form
  right_aligned_data = rev(right_aligned_data)

  ########################creation of DigitAnalysis class object########################

  DigitData = DigitAnalysis(raw=raw_data, cleaned=cleaned_data, numbers=numeric_data, left_aligned=left_aligned_data, right_aligned=right_aligned_data,
                            left_aligned_column_names=left_aligned_column_names, right_aligned_column_names=right_aligned_column_names, max=0)
  return(DigitData)
}

