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
# ptm <- proc.time()
# a=data.frame(matrix(1, nrow=100000000, ncol=10))
# proc.time() - ptm
# ptm <- proc.time()
# #a=c(1:1e6, 1:1e6)
# a=append(1:1e6, 1:1e6)
# proc.time() - ptm
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
N = 10000#100000 #120k datasets took 15 mins #2400s for N=10,000; data dimension = 4000 x 5 total digits
omit_05 = c(0,5)
break_out = 'DIST'
break_out_grouping=NA#list(a=c('Mandera', 'Isiolo', 'Baringo', 'Ijara'),b=c('Wajir', 'Garissa', 'Samburu', 'Marsabit', 'Moyale', 'Turkana', 'Tana'))
category= 'YEAR'#'SECTOR'#
category_grouping = NA#list(Training_and_Transport=c("TRN", "TRAVEL", "VEHICLES"), Civil_Works=c("CW"), Goods_and_Equipment=c("GE"))
distribution='benford'


ptm <- proc.time()
#match the data with Jetson's
result = padding_test(digitdata=DigitData, contingency_table=NA, data_columns=data_columns, max_length=max_length,
                      num_digits=num_digits, N=N, omit_05=omit_05, distribution=distribution, break_out=break_out,
                      break_out_grouping=break_out_grouping, category=category, category_grouping=category_grouping, plot=TRUE)
#result
proc.time() - ptm

#need a helper function to check category and category grouping validity############
# plot(result, type='l', xlab = 'Number of datasets / 5000', ylab = 'time (s)', main = 'Time to run N simulations')
# abline(a = -50, b = 80, lty = 2)
