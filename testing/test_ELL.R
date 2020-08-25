############################################################
# DigitAnalysis R Package
# github.com/jlederluis/digitanalysis
# Jetson Leder-Luis and Jean Ensminger
# Research assistant: Wenjun Chang
# Testing the 10 Tests in (Ensingmer & Leder-Luis, 2020)
############################################################

#clear workspace
rm(list = ls())
#free up R memory
gc()
#load stuff
devtools::install_github("https://github.com/jlederluis/digitanalysis", force = TRUE)

library(digitanalysis)
#filepath
fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv' #################

#Clean data: group training, travel, vehicles
D <- read.csv(fp, stringsAsFactors = FALSE)
D$SectorGroup <- D$SECTOR
unique(D$SECTOR)
D$SectorGroup[D$SectorGroup == "TRN" | D$SectorGroup == "TRAVEL" | D$SectorGroup == "VEHICLES"] <- "TRN_TRV_VEH"
# D <- D[D$SECTOR != "MICRO", ]
#data
Data = process_digit_data(raw_df = D, digit_columns = c('ALEXP.Values', "BENTOT.Values", "BENM", "BENF"))

#tests below:

#All digits test except first with expenditure
ADT_ALEXP = all_digits_test(digitdata = Data, data_columns = 'ALEXP.Values', skip_first_digit = TRUE, omit_05 = c(0,5), break_out='DIST',
                            suppress_first_division_plots=TRUE, plot=F)

#First digit test with expenditure
first_digit = all_digits_test(digitdata = Data, data_columns = 'ALEXP.Values', digit_places = 1, omit_05 = 0, break_out='DIST',
                              suppress_first_division_plots=TRUE, plot=F)

#All digits test except first with participants
ADT_BEN = all_digits_test(digitdata = Data, data_columns = c("BENM", "BENF"), skip_first_digit = TRUE, omit_05 = c(0,5), break_out='DIST',
                          suppress_first_division_plots=TRUE, plot=F)

#Digit pair test with participants
digit_pair = digit_pairs_test(digitdata = Data, data_columns = 'BENTOT.Values', omit_05 = 0, break_out='DIST', plot=F)

#Rounding test with expenditure
rounding = rounding_test(digitdata = Data, data_columns = 'ALEXP.Values', break_out='DIST',
                         rounding_patterns = c('0','00','000','0000', '00000', '000000', '5', '50', '500'), plot=F)

#Repeat test with expenditure
repeats = repeat_test(digitdata = Data, duplicate_matching_cols=c("ALEXP.Values", "YEAR", "DIST", "SECTOR"),
                      break_out='DIST', data_columns = 'ALEXP.Values', rounding_patterns_to_omit=c('000'), plot=F)

#Sector test with expenditure
sector = sector_test(digitdata = Data, category='SectorGroup', duplicate_matching_cols=c("ALEXP.Values", "YEAR", "DIST", "SECTOR"),
                     break_out='DIST', rounding_patterns_to_omit = '000', data_columns = 'ALEXP.Values',
                     category_instance_analyzing = 'TRN_TRV_VEH', plot=F)

#High low test with expenditure
high_low = high_low_test(digitdata = Data, data_columns = 'ALEXP.Values', omit_05 = c(0,5), skip_first_digit=TRUE, break_out='DIST', category='YEAR', plot=F)

#Unpack rounded numbers test with participants
unpack = unpack_round_numbers_test(digitdata = Data, rounding_split_column="BENTOT.Values", analysis_columns=c("BENM", "BENF"),
                                   skip_first_digit=TRUE, omit_05=c(0,5), break_out='DIST', suppress_first_division_plots=TRUE, plot=F)

#Padding test with expenditure
padding = padding_test(digitdata = Data, data_columns = 'ALEXP.Values', max_length=7, num_digits=5, N=10, omit_05=c(0,5), break_out='DIST', category='SectorGroup',
                       simulate=F, suppress_first_division_plots=TRUE, plot=T)



#filepath
fp = 'C:\\Users\\happy\\OneDrive - California Institute of Technology\\Desktop\\ARID MASTER FINAL.csv' #################
digitanalysis::replicate_EEL_10_tests(fp)

