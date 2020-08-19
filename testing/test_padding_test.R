# ############################################################
# #Testing for padding test
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
# data_columns = c("ALEXP.Values")
# fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv'
# DigitData = process_digit_data(filepath = fp, digit_columns = data_columns)
#
# #test padding test
# data_columns = c("ALEXP.Values")#c("ALEXP")#,"BENTOT", "BENM", "BENF")
# max_length = 7
# num_digits = 5
# N = 1#100000 #120k datasets took 15 mins #2400s for N=10,000; data dimension = 4000 x 5 total digits
# omit_05 = c(0,5)
# break_out = NA#'DIST'
# break_out_grouping=NA#list(a=c('Mandera', 'Isiolo', 'Baringo', 'Ijara'),b=c('Wajir', 'Garissa', 'Samburu', 'Marsabit', 'Moyale', 'Turkana', 'Tana'))
# category= NA#'YEAR'#'SECTOR'#
# category_grouping = NA#list(Training_and_Transport=c("TRN", "TRAVEL", "VEHICLES"), Civil_Works=c("CW"), Goods_and_Equipment=c("GE"))
# distribution='benford'
#
#
# ptm <- proc.time()
# #match the data with Jetson's
# result = padding_test(digitdata=DigitData, contingency_table=NA, data_columns=data_columns, max_length=max_length,
#                       num_digits=num_digits, N=N, omit_05=omit_05, distribution=distribution, break_out=break_out,
#                       break_out_grouping=break_out_grouping, category=category, category_grouping=category_grouping, plot=TRUE)
# result
# proc.time() - ptm
#
# #need a helper function to check category and category grouping validity############
# # plot(result, type='l', xlab = 'Number of datasets / 5000', ylab = 'time (s)', main = 'Time to run N simulations')
# # abline(a = -50, b = 80, lty = 2)
