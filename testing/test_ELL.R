############################################################
#Testing all 10 Tests in (Ensingmer & Leder-Luis, 2020)
#Wenjun Chang
#Summer 2020
############################################################

#clear workspace
rm(list = ls())
#free up R memory
gc()
#load stuff
library(digitanalysis)
#filepath
fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv' #################

#data
Data = process_digit_data(filepath = fp, digit_columns = c('ALEXP.Values', "BENTOT.Values", "BENM", "BENF"))
ALEXP = process_digit_data(filepath = fp, digit_columns = 'ALEXP.Values')
BENTOT = process_digit_data(filepath = fp, digit_columns = 'BENTOT.Values')
ADT_PARTICIPANTS = process_digit_data(filepath = fp, digit_columns =  c("BENM", "BENF"))
UNPACK_DATA = process_digit_data(filepath = fp, digit_columns = c("BENM", "BENF", "BENTOT.Values"))

#All digits test except first with expenditure
ADT_ALEXP = all_digits_test(digitdata = ALEXP, data_columns = 'ALEXP.Values', skip_first_digit = TRUE, omit_05 = c(0,5), break_out='DIST',
                            suppress_first_division_plots=TRUE, plot=F)

#First digit test with expenditure
first_digit = all_digits_test(digitdata = ALEXP, data_columns = 'ALEXP.Values', digit_places = 1, omit_05 = 0, break_out='DIST', suppress_first_division_plots=TRUE, plot=T)

#All digits test except first with participants
ADT_BEN = all_digits_test(digitdata = ADT_PARTICIPANTS, data_columns = c("BENM", "BENF"), skip_first_digit = TRUE, omit_05 = c(0,5), break_out='DIST',
                          suppress_first_division_plots=TRUE, plot=F)

#Digit pair test with participants
digit_pair = digit_pairs_test(digitdata = BENTOT, data_columns = 'BENTOT.Values', omit_05 = 0, break_out='DIST', plot=T)

#Rounding test with expenditure
rounding = rounding_test(digitdata = ALEXP, data_columns = 'ALEXP.Values', break_out='DIST', plot=T, rounding_patterns = c('0','00','000','0000', '00000', '000000', '5', '50', '500'))

#Repeat test with expenditure
repeats = repeat_test(digitdata = ALEXP, duplicate_matching_cols=c("ALEXP.Values", "YEAR", "DIST", "SECTOR"),
                      break_out='DIST', data_columns = 'ALEXP.Values', rounding_patterns_to_omit=c('000'), plot=T)

#Sector test with expenditure
sector = sector_test(digitdata = ALEXP, category='SECTOR', duplicate_matching_cols=c("ALEXP.Values", "YEAR", "DIST", "SECTOR"),
                     break_out='DIST', rounding_patterns_to_omit = '000', data_columns = 'ALEXP.Values',
                     category_instance_analyzing = 'TRN', plot=T) #sector test hasnt crerate a new sector grouping col in original data

#High low test with expenditure
high_low = high_low_test(digitdata = ALEXP, omit_05 = c(0,5), skip_first_digit=TRUE, break_out='DIST', category='YEAR', plot=F)

#Unpack rounded numbers test with participants
unpack = unpack_round_numbers_test(digitdata = UNPACK_DATA, rounding_split_column="BENTOT.Values", analysis_columns=c("BENM", "BENF"),
                                   skip_first_digit=TRUE, omit_05=c(0,5), break_out='DIST', suppress_first_division_plots=TRUE, plot=F)

#Padding test with expenditure
padding = padding_test(digitdata = ALEXP, data_columns = 'ALEXP.Values', max_length=7, num_digits=5, N=10, omit_05=c(0,5), break_out='DIST', category='SECTOR',
                       category_grouping=list(Training_and_Transport=c("TRN", "TRAVEL", "VEHICLES"), Civil_Works=c("CW"), Goods_and_Equipment=c("GE")),
                       simulate=T, plot=F)

