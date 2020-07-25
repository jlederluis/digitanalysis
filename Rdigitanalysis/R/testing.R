# ############################################################
# #Testing; run the functions
# #Wenjun Chang
# #Summer 2020
# ############################################################
#
#
# #############prelim############
# #clear workspace
# rm(list = ls())
# #free up R memory
# gc()
# #force numerical representation rather than scientific
# # options(scipen = 999)
# # options(scipen = 1)
# # options(digits = 2)
# ##############################
#
#
# #load data input functions
# source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\data_input_functions.R')
#
# #load functions for computing Benford table
# source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\Benford_table_functions.R')
#
# #load helper functions for all digit test
# source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\all_digit_test_helper_functions.R')
#
# #load chi square test GOF functions
# source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\chi_square_goodness_of_fit_functions.R')
#
# #load main function for all digit test
# source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\all_digit_test_main_function.R')
#
# #load functionS for unpack round numbers test
# source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\unpack_round_numbers_test.R')
#
# #load all functions for digit pair test
# source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\digit_pair_test.R')
#
# #load all functions for rounding test
# source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\rounding_test.R')
#
# #load all functions for repeat test
# source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\repeat_test.R')
#
# #load all functions for high low test
# source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\high_low_test.R')
#
# #load all functions for padding test
# source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\padding_test.R')
#
# #load all plotting functions
# source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\plotting_functions.R')
#
# #load input check function
# source('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\Rdigitanalysis\\R\\input_check_function.R')
#
#
# #############################################################
# #############try it with given data##########################
# #############################################################
#
# #test data input and benford table functions
# #load data input functions
# data_columns = c("ALEXP.Values")#,"BENTOT", "BENM", "BENF")
# fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL MinData.csv'
#
# test = read.csv('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL MinData.csv')
#
# # omitting_index = which(is.na(test$ALEXP.Values))
#
#
# DigitData = make_class(filepath = fp, col_analyzing = data_columns)
#
#
#
# #a[which(a$`ALEXP.Values 6th digit` ==6),]
# #b=a[which(a$`ALEXP.Values 5th digit` ==6),]
#
# # omitting_index
# #
# # DigitData_match = DigitData
# # DigitData_match@cleaned = DigitData_match@cleaned[-omitting_index, ]
# # DigitData_match@numbers = data.frame(DigitData_match@numbers[-omitting_index, ])
# # colnames(DigitData_match@numbers) = colnames(DigitData@numbers)
# #
# # DigitData_match@left_aligned = DigitData_match@left_aligned[-omitting_index, ]
# # DigitData_match@right_aligned = DigitData_match@right_aligned[-omitting_index, ]
# #
# # a=test$ALEXP.Values
# # length(which(!(is.na(a))))
# # length(omitting_index)+length(which(!(is.na(a))))
# # dim(DigitData_match@cleaned)
#
# contingency_table = load_Benford_table('C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\digitanalysis\\contingency_table.csv')
# # contingency_table = contingency_table[!colnames(contingency_table) %in% c('a', 'X', 'Digits')]
# # contingency_table
# # saveRDS(contingency_table, file = "data/benford_table.RData")
# # #contingency table is here
# # readRDS(file = "data/benford_table.RData")
#
#
# #test all digits test
# data_columns = 'ALEXP.Values'
# digit_places =  c(2,3,4,5,6)#'all'# c(1,2,3)
# skip_first_digit=TRUE
# omit_05 = c(0,5)
# break_out="ï..DIST"#'DIST'
# # distribution='Benford'
# # plot=TRUE
# skip_last_digit=FALSE
#
# #match the data with Jetson's
# result = all_digits_test(digitdata = DigitData, contingency_table = contingency_table, data_columns = data_columns, digit_places = digit_places,
#                          skip_first_digit = skip_first_digit, omit_05 = omit_05, break_out=break_out, distribution='Benford', plot=TRUE, skip_last_digit = skip_last_digit)
#
# order = c('Mandera', 'Isiolo', 'Baringo', 'Ijara', 'Wajir', 'Garissa', 'Samburu', 'Marsabit', 'Moyale', 'Turkana', 'Tana', 'all')
# result[order]
#
#
# # if error must be in usable data
#
# # count = 0
# # for (i in 1:dim(result)[1]){
# #   for (j in 1:dim(result)[2]){
# #     ele = result[i,j]
# #     if (!is.na(ele)){
# #       if (ele != 0){
# #         if (ele != 5)
# #           count= count + 1
# #       }
# #     }
# #   }
# # }
# # count
#
#
#
#
# result = all_digits_test(digitdata = DigitData, contingency_table = contingency_table, data_columns = data_columns, digit_places = digit_places,
#                          skip_first_digit = skip_first_digit, omit_05 = omit_05, break_out=break_out, distribution='Benford', plot=TRUE, skip_last_digit = skip_last_digit)
#
#
# order = c('Mandera', 'Isiolo', 'Baringo', 'Ijara', 'Wajir', 'Garissa', 'Samburu', 'Marsabit', 'Moyale', 'Turkana', 'Tana', 'all')
# result[order]
#
# DigitData_match = DigitData
#
# c=DigitData_match@cleaned$DIST
# index=which(c=='Baringo')
# d=DigitData_match@cleaned$'ALEXP.Values'
# length(d[index])
# baringo_numbers = d[index]
# # count num digits
# sum(floor(log10(baringo_numbers)) + 1, na.rm = T)
# baringo_digits = single_column_aligned(digitdata = DigitData_match, desired_col = 'ALEXP', align_diretion = 'left')[index,]
# length(baringo_digits[,1])
#
# table(DigitData@left_aligned[5])
#
#
#
#
# #skip first
#
#
#
# N = 0
# ddd = DigitData@left_aligned[2:7]
# for (col in ddd){
#   N = N + sum((table(col)[!names(table(col)) %in% c('5', '0')]))
# }
# N
#
# d = DigitData@numbers[[1]]
# num_digits = 0
# baringo_digits = strsplit(as.character(baringo_numbers), '')
# all = strsplit(as.character(d), '')
# excel = strsplit(as.character(test$ALEXP.Values), '')
# #excel = strsplit(as.character(DigitData@numbers), '')
#
# for (i in all){
#   # # count first digit
#   # if (! i[1] %in% c('5', '0')){
#   # num_digits = num_digits + 1
#   # }
#   # skip first
#   if (!(is.na(i))){
#     ii = i[-1]
#     iii = ii[!ii %in% c('5', '0')]
#     num_digits = num_digits + length(iii)
#     print(iii)
#   }
#
# }
# num_digits
#
#
#
# numbers = test$ALEXP.Values[which(!is.na(test$ALEXP.Values))]
# length(numbers)
# table(numbers == d)
#
# # baringo_digits = baringo_digits[-c(1)]
# # length(baringo_digits[baringo_digits != 5][baringo_digits != 0][!is.na(baringo_digits)])
#
#
#
# #test unpack round numbers test
# unpacking_rounding_column='ALEXP'
# data_columns = 'all'#'ALEXP'
# digit_places =  'all'# c(1,2,3)
# skip_first_digit=TRUE
# omit_05 = c(0,5)
# break_out='DIST'
# # distribution='Benford'
# # plot=TRUE
# skip_last_digit=FALSE
#
# unpack = unpack_round_numbers_test(digitdata = DigitData, contingency_table = contingency_table, unpacking_rounding_column = unpacking_rounding_column, data_columns = data_columns,
#                            digit_places = digit_places, skip_first_digit = skip_first_digit, omit_05 = omit_05, break_out=break_out, distribution='Benford', plot=TRUE,
#                            skip_last_digit = skip_last_digit)
#
#
# #test rounding test
# omit_05 = c(0,5)
# data_columns = c("ALEXP")#,"BENTOT", "BENM", "BENF")
# break_out = 'DIST'
# rounding_test(DigitData, data_columns, omit_05, break_out)
#
#
#
# #test digit pair test
# min_length = 3
# omit_05 = NA
# data_columns = c("ALEXP","BENTOT", "BENM", "BENF")
# break_out = 'DIST'
#
# digit_pairs_test(DigitData, data_columns, omit_05, min_length, break_out)
#
#
#
# #test repeat test
# duplicate_matching_cols = c('ALEXP', 'DIST', 'BENTOT')
# break_out = 'DIST'
# data_columns = 'all'
# sector_column = 'SECTOR'
# sector_grouping = list(Sector1=c("MICRO", "TRN"), Sector2=c("CW" , "GE"), Sector3=c("TRAVEL", "VEHICLES"))
# # failure_factor = 3
#
# sector_grouping
#
# repeat_test(DigitData, data_columns, duplicate_matching_cols, break_out=break_out, sector_column=sector_column, sector_grouping=sector_grouping, failure_factor=3)
#
#
# #test high low test
# data_columns = c("ALEXP", "BENTOT")
# high = c(6,7,8,9)
# omit_05 = c(0,5)
# skip_first_digit = TRUE
# skip_last_digit = FALSE
# break_out = 'YEAR'
#
# high_low_test(DigitData, contingency_table, data_columns, high, omit_05, skip_first_digit, skip_last_digit, break_out)
#
#
#
# #test padding test
# data_columns = c("ALEXP")#,"BENTOT", "BENM", "BENF")
# max_length = 7
# num_digits = 5
# N = 10 #120k datasets took 15 mins
# omit_05 = c(0,5)
# break_out = 'DIST'
#
# a=padding_test(DigitData, contingency_table, data_columns, max_length, num_digits, N, omit_05, break_out)
# a
# data.frame(t(a$diff_in_mean))
# a$p_values
# a$expected_mean
# a$observed_mean
#
#
# #test plot
# data_columns = c("ALEXP")#,"BENTOT", "BENM", "BENF")
# max_length = 7
# num_digits = 5
# N = 10
# omit_05 = c(0,5)
#
# test=padding_test(DigitData, contingency_table, data_columns, max_length, num_digits, N, omit_05, break_out=NA)$diff_in_mean
# s=hist_2D(test, hline=mean(as.matrix(test)))
# s
#
# #2d with variables test
# test2=padding_test(DigitData, contingency_table, data_columns, max_length, num_digits, N, omit_05, break_out="DIST")$diff_in_mean[1:3, ]
# a=hist_2D_variables(test2)
# a
#
# #test multiple plots
# plots = list(a, a, a, a)
# plot_multiple_hist2d(plots)
#
#
