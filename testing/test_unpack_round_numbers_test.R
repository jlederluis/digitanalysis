# ############################################################
# #Testing for all digit test and unpack unround column tests
# #Wenjun Chang
# #Summer 2020
# ############################################################
#
# #############prelim############
# #clear workspace
# rm(list = ls())
# #free up R memory
# gc()
# #load stuff
# library(digitanalysis)
# ##############################
#
# #test with data
# #load data input functions
# data_columns = c("BENM", "BENF")#, "BENTOT.Values")
# fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv'
# DigitData = process_digit_data(filepath = fp, digit_columns = data_columns)
#
# #test unpack rounde test
# data_columns = "all"
# digit_places =  'all'
# skip_first_digit=TRUE
# omit_05 = c(0,5)
# break_out=NA#'DIST'
# skip_last_digit=FALSE
# standard_df=TRUE
# suppress_low_N=FALSE
# unpacking_rounding_column="BENTOT.Values"
# distribution = 'benford'
# category=NA
# category_grouping=NA
#
# #match the data with Jetson's
# result = unpack_round_numbers_test(digitdata=DigitData, contingency_table=NA, unpacking_rounding_column=unpacking_rounding_column, data_columns=data_columns,
#                                    digit_places=digit_places, skip_first_digit=skip_first_digit, omit_05=omit_05, break_out=break_out, category=category,
#                                    category_grouping=category_grouping,distribution=distribution, plot=T, skip_last_digit=skip_last_digit,
#                                    standard_df=standard_df, suppress_low_N=suppress_low_N)
#
# #order = c('Mandera', 'Isiolo', 'Baringo', 'Ijara', 'Wajir', 'Garissa', 'Samburu', 'Marsabit', 'Moyale', 'Turkana', 'Tana', 'all')
# #result[order]

