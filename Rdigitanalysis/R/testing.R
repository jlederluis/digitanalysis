############################################################
#Tesiting; run the fucntions
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
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\data input functions.R')

#load functions for computing Benford table
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\Benford table functions.R')

#load helper functions for all digit test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\all digit test helper functions.R')

#load chi square test GOF functions
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\chi square goodness of fit functions.R')

#load main function for all digit test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\all digit test main function.R')

#load all functions for digit pair test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\digit pair test.R')

#load all functions for rounding test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\rounding test.R')

#load all functions for repeat test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\repeat test.R')

#load all functions for high low test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\high low test.R')

#load all functions for padding test
source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\padding test.R')


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
digit_places = c(1)#c(1,2,3)
look_or_omit = 'look'
skip_first_figit=FALSE
omit_05 = c(0,5)
break_out='DIST'
# distribution='Benford'
# plot=TRUE
last_digit_test_included=FALSE
unpacking_rounding_column='ALEXP'

result = all_digits_test(digitdata = DigitData, contingency_table = contingency_table, data_columns = data_columns, digit_places = digit_places, look_or_omit = look_or_omit,
                skip_first_figit = skip_first_figit, omit_05 = omit_05, break_out=break_out, distribution='Benford', plot=TRUE,
                last_digit_test_included=FALSE, unpacking_rounding_column=unpacking_rounding_column)



#test rounding test
omit_05 = c(0,5)
data_columns = c("ALEXP")#,"BENTOT", "BENM", "BENF")
break_out = 'DIST'
rounding_test(DigitData, data_columns, omit_05, break_out)



#test digit pair test
min_length = 3
last_digit_test_included = TRUE
omit_05 = NA
data_columns = c("ALEXP")#,"BENTOT", "BENM", "BENF")
break_out = 'DIST'

digit_pairs_test(DigitData, data_columns, omit_05, min_length, last_digit_test_included, break_out)



#test repeat test
duplicate_matching_cols = c('ALEXP', 'DIST', 'BENTOT')
remove_duplicate = TRUE
break_out = 'DIST'

sector_column = NA#'SECTOR'
sector_grouping = NA#list(Sector1=c("MICRO", "TRN"), Sector2=c("CW" , "GE"), Sector3=c("TRAVEL", "VEHICLES"))
# failure_factor = 3

sector_grouping

repeat_test(DigitData, duplicate_matching_cols, remove_duplicate=remove_duplicate, break_out=break_out, sector_column=sector_column, sector_grouping=sector_grouping, failure_factor=3)


#test high low test
data_columns = c("ALEXP", "BENTOT")
high = c(6,7,8,9)
omit_05 = c(0,5)
skip_first_figit = TRUE
last_digit_test_included = FALSE
break_out = 'YEAR'

high_low_test(DigitData, contingency_table, data_columns, high, omit_05, skip_first_figit, last_digit_test_included, break_out)



#test padding test
data_columns = c("ALEXP")#,"BENTOT", "BENM", "BENF")
max_length = 7
num_digits = 5
N = 10 #120k datasets took 15 mins
omit_05 = c(0,5)
break_out = 'DIST'

a=padding_test(DigitData, contingency_table, data_columns, max_length, num_digits, N, omit_05, break_out)
a
a$diff_in_mean
a$p_values
a$expected_mean
a$observed_mean


