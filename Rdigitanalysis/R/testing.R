############################################################
#Testing; run the functions
#Wenjun Chang
#Summer 2020
############################################################


#############prelim############
#clear workspace
rm(list = ls())
#free up R memory
gc()
#force numerical representation rather than scientific
#options(scipen = 999)
options(scipen = 1)
options(digits = 2)
##############################


#load data input functions
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\data_input_functions.R')

#load functions for computing Benford table
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\Benford_table_functions.R')

#load helper functions for all digit test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\all_digit_test_helper_functions.R')

#load chi square test GOF functions
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\chi_square_goodness_of_fit_functions.R')

#load main function for all digit test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\all_digit_test_main_function.R')

#load functionS for unpack round numbers test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\unpack_round_numbers_test.R')

#load all functions for digit pair test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\digit_pair_test.R')

#load all functions for rounding test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\rounding_test.R')

#load all functions for repeat test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\repeat_test.R')

#load all functions for high low test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\high_low_test.R')

#load all functions for padding test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\padding_test.R')

#load all plotting functions
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\plotting_functions.R')


#############################################################
#############try it with given data##########################
#############################################################

#test data input and benford table functions
#load data input functions
data_columns = c("ALEXP","BENTOT", "BENM", "BENF")
fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv'

DigitData = make_class(filepath = fp, col_analyzing = data_columns)

contingency_table = load_Benford_table('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\contingency_table.csv')
contingency_table


#test all digits test
data_columns = 'all'#'ALEXP'
digit_places =  'all'# c(1,2,3)
skip_first_figit=TRUE
omit_05 = c(0,5)
break_out='DIST'
# distribution='Benford'
# plot=TRUE
skip_last_digit=FALSE

result = all_digits_test(digitdata = DigitData, contingency_table = contingency_table, data_columns = data_columns, digit_places = digit_places,
                         skip_first_figit = skip_first_figit, omit_05 = omit_05, break_out=break_out, distribution='Benford', plot=TRUE, skip_last_digit = skip_last_digit)


#test unpack round numbers test
unpacking_rounding_column='ALEXP'
data_columns = 'all'#'ALEXP'
digit_places =  'all'# c(1,2,3)
skip_first_figit=TRUE
omit_05 = c(0,5)
break_out='DIST'
# distribution='Benford'
# plot=TRUE
skip_last_digit=FALSE

unpack = unpack_round_numbers_test(digitdata = DigitData, contingency_table = contingency_table, unpacking_rounding_column = unpacking_rounding_column, data_columns = data_columns,
                           digit_places = digit_places, skip_first_figit = skip_first_figit, omit_05 = omit_05, break_out=break_out, distribution='Benford', plot=TRUE,
                           skip_last_digit = skip_last_digit)


#test rounding test
omit_05 = c(0,5)
data_columns = c("ALEXP")#,"BENTOT", "BENM", "BENF")
break_out = 'DIST'
rounding_test(DigitData, data_columns, omit_05, break_out)



#test digit pair test
min_length = 3
omit_05 = NA
data_columns = c("ALEXP","BENTOT", "BENM", "BENF")
break_out = 'DIST'

digit_pairs_test(DigitData, data_columns, omit_05, min_length, break_out)



#test repeat test
duplicate_matching_cols = c('ALEXP', 'DIST', 'BENTOT')
break_out = 'DIST'
data_columns = 'all'
sector_column = 'SECTOR'
sector_grouping = list(Sector1=c("MICRO", "TRN"), Sector2=c("CW" , "GE"), Sector3=c("TRAVEL", "VEHICLES"))
# failure_factor = 3

sector_grouping

repeat_test(DigitData, data_columns, duplicate_matching_cols, break_out=break_out, sector_column=sector_column, sector_grouping=sector_grouping, failure_factor=3)


#test high low test
data_columns = c("ALEXP", "BENTOT")
high = c(6,7,8,9)
omit_05 = c(0,5)
skip_first_figit = TRUE
skip_last_digit = FALSE
break_out = 'YEAR'

high_low_test(DigitData, contingency_table, data_columns, high, omit_05, skip_first_figit, skip_last_digit, break_out)



#test padding test
data_columns = c("ALEXP")#,"BENTOT", "BENM", "BENF")
max_length = 7
num_digits = 5
N = 10 #120k datasets took 15 mins
omit_05 = c(0,5)
break_out = 'DIST'

a=padding_test(DigitData, contingency_table, data_columns, max_length, num_digits, N, omit_05, break_out)
a
data.frame(t(a$diff_in_mean))
a$p_values
a$expected_mean
a$observed_mean


#test plot
data_columns = c("ALEXP")#,"BENTOT", "BENM", "BENF")
max_length = 7
num_digits = 5
N = 10
omit_05 = c(0,5)

test=padding_test(DigitData, contingency_table, data_columns, max_length, num_digits, N, omit_05, break_out=NA)$diff_in_mean
s=hist_2D(test, hline=mean(as.matrix(test)))
s

#2d with variables test
test2=padding_test(DigitData, contingency_table, data_columns, max_length, num_digits, N, omit_05, break_out="DIST")$diff_in_mean[1:3, ]
a=hist_2D_variables(test2)
a

#test multiple plots
plots = list(a, a, a, a)
plot_multiple_hist2d(plots)


