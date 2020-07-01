############################################################
#Functions for digit analysis R package
#Wenjun Chang
#Summer 2020
############################################################


#clear workspace
rm(list = ls())
#force numerical representation rather than scientific
options(scipen = 999)


############################################################
#Class function
############################################################


#create our own class
DigitAnalysis = setClass('DigitAnalysis', slots = c(raw="data.frame", cleaned="data.frame",
                                                    numbers="data.frame", left_aligned="data.frame",
                                                    right_aligned="data.frame"))


############################################################
#helper functions
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
    print(paste(colname, naming_method[i]))
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
  }
  return(outdata)
}


############################################################
#Data input main function
############################################################

#parse and clean the data for digit analysis
make_class = function(filepath, data_columns){
  #data format: row is observation; column is category
  #raw input data->'raw' of the class
  raw_data = read.csv(filepath)

  #clean up the number columns with numeric data to be analyzed->'cleaned' of the class
  cleaned_data = data.frame(raw_data) #make copy without pointer issue
  cleaned_data = drop_nan_empty(cleaned_data, col_analyzing)

  #the dataframe with only the data being analyzed->'numeric' of the class
  numeric_data = data.frame(matrix(ncol = 0, nrow = length(cleaned_data[,1])))

  #the dataframe with the left aligned digits of each data column to be analyzed->'left_aligned' of the class
  #i.e. a column is 'X'; first digit will be at column '1st digit' + 'X'
  left_aligned_column_names = c('1st digit', '2nd digit', '3rd digit', '4th digit', '5th digit', '6th digit', '7th digit',
                                '8th digit', '9th digit', '10th digit', '11th digit', '12th digit', '13th digit')
  left_aligned_data = data.frame(matrix(ncol = 0, nrow = length(cleaned_data[,1])))

  #the dataframe with the right aligned digits of each data column to be analyzed->'right_aligned' of the class
  #i.e. a column is 'X'; first digit will be at column '1s' + 'X'
  right_aligned_column_names = c('1s', '10s', '100s', '1k', '10k', '100k', '1m', '10m', '100m', '1b', '10b', '100b', '1t')
  right_aligned_data = data.frame(matrix(ncol = 0, nrow = length(cleaned_data[,1])))

  ##can be specified as both numeric_data='sdfsfsf' or numeric_data=c('dsdfsfsf') or or numeric_data=c('dsdfsfsf',...)
  for (i in 1:length(col_analyzing)) {
    #name of current data column modifying
    col_name = col_analyzing[i]

    #turn into numbers
    cleaned_data[[col_name]] = as.numeric(gsub(",","",cleaned_data[[col_name]]))

    #rounding
    cleaned_data[[col_analyzing[i]]] = round_data(cleaned_data[[col_name]])

    #update 'numeric' object
    numeric_data[[col_name]] = NA
    numeric_data[[col_name]] = cleaned_data[[col_name]]

    #update 'align_left'
    left_aligned_data = align_digits(indata=cleaned_data[[col_name]], outdata=left_aligned_data, naming_method=left_aligned_column_names, align_direction='left', colname=col_name)

    #update 'align_right'
    right_aligned_data = align_digits(indata=cleaned_data[[col_name]], outdata=right_aligned_data, naming_method=right_aligned_column_names, align_direction='right', colname=col_name)
  }

  #make our own class object finanlly!
  DigitData = DigitAnalysis(raw = raw_data, cleaned = cleaned_data, numbers=numeric_data, left_aligned=left_aligned_data, right_aligned=right_aligned_data)

  return(DigitData)
}

#############try it with given data
col_analyzing = c("ALEXP","BENTOT", "BENM", "BENF")
fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv'

DigitData = make_class(filepath = fp, data_columns = col_analyzing)
head(DigitData@raw,2)
head(DigitData@cleaned,2)
head(DigitData@numbers,2)
a=head(DigitData@left_aligned,2)
b=head(DigitData@right_aligned,2)

######
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
