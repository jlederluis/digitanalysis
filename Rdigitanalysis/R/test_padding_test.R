############################################################
#Testing for padding test
#Wenjun Chang
#Summer 2020
############################################################

#############prelim############
#clear workspace
rm(list = ls())
#free up R memory
gc()
##############################
#general functions
#load data input functions
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\data_input_functions.R')

#load functions for computing Benford table
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\Benford_table_functions.R')

#load all plotting functions
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\plotting_functions.R')

#load input check function
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\input_check_function.R')

############################
#testing
#load helper functions for all digit test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\all_digit_test_helper_functions.R')

#load all functions for padding test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\padding_test.R')


#test with data
#load data input functions
data_columns = c("ALEXP.Values")
fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv'

DigitData = make_class(filepath = fp, col_analyzing = data_columns)
contingency_table = load_Benford_table('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\contingency_table.csv')

#DigitData has to drop all columns with NA in ALEXP.Values
indexes_with_valid_alexp_values = which(!(is.na(DigitData@cleaned$ALEXP.Values)))
DigitData = make_sub_digitdata(DigitData, indexes_with_valid_alexp_values)

#test padding test
data_columns = c("ALEXP.Values")#c("ALEXP")#,"BENTOT", "BENM", "BENF")
max_length = 7
num_digits = 5
N = 100000 #120k datasets took 15 mins #2400s for N=10,000; data = 4000 x 5, all data
omit_05 = c(0,5)
break_out = NA#'DIST'
break_out_grouping=NA
category= NA#'SECTOR'
category_grouping = NA#list(Training_and_Transport=c("TRN", "TRAVEL", "VEHICLES"), Civil_Works=c("CW"), Goods_and_Equipment=c("GE"))
distribution='benford'


ptm <- proc.time()

#match the data with Jetson's
result = padding_test(digitdata=DigitData, contingency_table=NA, data_columns=data_columns, max_length=max_length,
                      num_digits=num_digits, N=N, omit_05=omit_05, distribution=distribution, break_out=break_out,
                      break_out_grouping=break_out_grouping, category=category, category_grouping=category_grouping, plot=TRUE)
result
proc.time() - ptm



