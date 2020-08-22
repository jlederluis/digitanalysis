# ############################################################
# #Testing all 10 Tests in (Ensingmer & Leder-Luis, 2020)
# #Wenjun Chang
# #Summer 2020
# ############################################################
#
# library(digitanalysis)
#
# #filepath
# fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv' #################
#
# #data
# ALEXP = process_digit_data(filepath = fp, digit_columns = 'ALEXP.Values')
# BENTOT = process_digit_data(filepath = fp, digit_columns = 'BENTOT.Values')
# ADT_PARTICIPANTS = process_digit_data(filepath = fp, digit_columns =  c("BENM", "BENF"))
# UNPACK_DATA = process_digit_data(filepath = fp, digit_columns = c("BENM", "BENF", "BENTOT.Values"))
#
# #1. All digits test except first with expenditure
# ADT_ALEXP = all_digits_test(digitdata = ALEXP, skip_first_digit = TRUE, omit_05 = c(0,5), break_out='DIST', suppress_first_division_plots=TRUE, plot=T)
#
# #2. First digit test with expenditure
# first_digit = all_digits_test(digitdata = ALEXP, digit_places = 1, omit_05 = 0, break_out='DIST', suppress_first_division_plots=TRUE, plot=T)
#
# #3. All digits test except first with participants
# ADT_BENTOT = all_digits_test(digitdata = ADT_PARTICIPANTS, skip_first_digit = TRUE, omit_05 = c(0,5), break_out='DIST', suppress_first_division_plots=TRUE, plot=T)
#
# #4. Digit pair test with participants
# digit_pair = digit_pairs_test(digitdata = BENTOT, omit_05 = 0, break_out='DIST', plot=T)
#
# #5. Rounding test with expenditure
# rounding = rounding_test(digitdata = ALEXP, break_out='DIST', plot=T)
#
# #6. Repeat test with expenditure
# repeats = repeat_test(digitdata = ALEXP, duplicate_matching_cols=c("ALEXP.Values", "YEAR", "DIST", "SECTOR"), break_out='DIST', plot=T)
#
# #7. Sector test with expenditure
# sector = sector_test(digitdata = ALEXP, category='SECTOR', category_grouping=list(Training_and_Transport=c("TRN", "TRAVEL", "VEHICLES"), Civil_Works=c("CW"), Goods_and_Equipment=c("GE")),
#                      duplicate_matching_cols=c("ALEXP.Values", "YEAR", "DIST", "SECTOR"), break_out='DIST', plot=T)
#
# #8. High low test with expenditure
# high_low = high_low_test(digitdata = ALEXP, omit_05 = c(0,5), skip_first_digit=TRUE, break_out='DIST', category='YEAR', plot=F)
#
# #9. Unpack rounded numbers test with participants
# unpack = unpack_round_numbers_test(digitdata = UNPACK_DATA, unpacking_rounding_column="BENTOT.Values", data_columns=c("BENM", "BENF"),
#                                    skip_first_digit=TRUE, omit_05=c(0,5), break_out='DIST', suppress_first_division_plots=TRUE, plot=F)
#
# #10. Padding test with expenditure
# padding = padding_test(digitdata = ALEXP, max_length=7, num_digits=5, N=10, omit_05=c(0,5), break_out='DIST', category='SECTOR',
#                        category_grouping=list(Training_and_Transport=c("TRN", "TRAVEL", "VEHICLES"), Civil_Works=c("CW"), Goods_and_Equipment=c("GE")),
#                        simulate=FALSE, plot=F)
#
