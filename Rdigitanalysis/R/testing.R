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


#############################################################
#############try it with given data##########################
#############################################################


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
omit_05 = c(0,5)
break_out='DIST'
# distribution='Benford'
# plot=TRUE
last_digit_test_included=FALSE
unpacking_rounding_column='ALEXP'

all_digits_test(digitdata = DigitData, contingency_table = contingency_table, data_columns = data_columns, digit_places = digit_places, look_or_omit = look_or_omit,
                skip_first_figit = skip_first_figit, omit_05 = omit_05, break_out=break_out, distribution='Benford', plot=TRUE,
                last_digit_test_included=FALSE, unpacking_rounding_column=unpacking_rounding_column)
