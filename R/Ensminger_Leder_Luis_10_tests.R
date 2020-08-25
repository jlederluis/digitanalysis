############################################################
# DigitAnalysis R Package
# github.com/jlederluis/digitanalysis
# Jetson Leder-Luis and Jean Ensminger
# Research assistant: Wenjun Chang
# Function to run and return relevant tables and plots for the 10 Tests in (Ensingmer & Leder-Luis, 2020)
############################################################


#' Replicates Ensingmer & Leder-Luis 10 Tests
#'
#' @param fp filepath for input data
#' @param raw_df dataframe for input data
#' @param plot TRUE or FALSE: If generate plot or not. Default to TRUE, might have bugs if set to FALSE lmao.
#'
#' @return The test statistics table and the plots for Ensingmer & Leder-Luis 10 Tests
#' @export
#'
#' @examples
#' replicate_EEL_10_tests(fp=filepath)
#' replicate_EEL_10_tests(raw_df=dataframe)
replicate_EEL_10_tests = function(fp=NA, raw_df=NA, plot=TRUE){
  #data
  Data = NA
  if (!is.na(fp)){
    D <- read.csv(fp, stringsAsFactors = FALSE)
    D$SectorGroup <- D$SECTOR
    unique(D$SECTOR)
    D$SectorGroup[D$SectorGroup == "TRN" | D$SectorGroup == "TRAVEL" | D$SectorGroup == "VEHICLES"] <- "TRN_TRV_VEH"
    # D <- D[D$SECTOR != "MICRO", ]
    Data = process_digit_data(raw_df = D, digit_columns = c('ALEXP.Values', "BENTOT.Values", "BENM", "BENF"))
  }
  else {
    Data = process_digit_data(raw_df = raw_df, digit_columns = c('ALEXP.Values', "BENTOT.Values", "BENM", "BENF"))
  }

  #All digits test except first with expenditure
  ADT_ALEXP = all_digits_test(digitdata = Data, data_columns = 'ALEXP.Values', skip_first_digit = TRUE, omit_05 = c(0,5), break_out='DIST',
                              suppress_first_division_plots=TRUE, plot=plot)

  #First digit test with expenditure
  first_digit = all_digits_test(digitdata = Data, data_columns = 'ALEXP.Values', digit_places = 1, omit_05 = 0, break_out='DIST',
                                suppress_first_division_plots=TRUE, plot=plot)

  #All digits test except first with participants
  ADT_BEN = all_digits_test(digitdata = Data, data_columns = c("BENM", "BENF"), skip_first_digit = TRUE, omit_05 = c(0,5), break_out='DIST',
                            suppress_first_division_plots=TRUE, plot=plot)

  #Digit pair test with participants
  digit_pair = digit_pairs_test(digitdata = Data, data_columns = 'BENTOT.Values', omit_05 = 0, break_out='DIST', plot=plot)

  #Rounding test with expenditure
  rounding = rounding_test(digitdata = Data, data_columns = 'ALEXP.Values', break_out='DIST',
                           rounding_patterns = c('0','00','000','0000', '00000', '000000', '5', '50', '500'), plot=plot)

  #Repeat test with expenditure
  repeats = repeat_test(digitdata = Data, duplicate_matching_cols=c("ALEXP.Values", "YEAR", "DIST", "SECTOR"),
                        break_out='DIST', data_columns = 'ALEXP.Values', rounding_patterns_to_omit=c('000'), plot=plot)

  #Sector test with expenditure
  sector = sector_test(digitdata = Data, category='SectorGroup', duplicate_matching_cols=c("ALEXP.Values", "YEAR", "DIST", "SECTOR"),
                       break_out='DIST', rounding_patterns_to_omit = '000', data_columns = 'ALEXP.Values',
                       category_instance_analyzing = 'TRN_TRV_VEH', plot=plot)

  #High low test with expenditure
  high_low = high_low_test(digitdata = Data, data_columns = 'ALEXP.Values', omit_05 = c(0,5), skip_first_digit=TRUE, break_out='DIST', category='YEAR', plot=plot)

  #Unpack rounded numbers test with participants
  unpack = unpack_round_numbers_test(digitdata = Data, rounding_split_column="BENTOT.Values", analysis_columns=c("BENM", "BENF"),
                                     skip_first_digit=TRUE, omit_05=c(0,5), break_out='DIST', suppress_first_division_plots=TRUE, plot=plot)

  #Padding test with expenditure
  padding = padding_test(digitdata = Data, data_columns = 'ALEXP.Values', max_length=7, num_digits=5, N=10, omit_05=c(0,5), break_out='DIST', category='SectorGroup',
                         simulate=FALSE, suppress_first_division_plots=TRUE, plot=plot)
  result_table = data.frame(matrix(nrow=0, ncol=nrow(unpack$round)))
  colnames(result_table) = rownames(unpack$round)
  result_table['All Digits Test: Expenditure', ][rownames(ADT_ALEXP$p_values)] = ADT_ALEXP$p_values[rownames(ADT_ALEXP$p_values), ]
  result_table['First Digit Test: Expenditure', ][rownames(first_digit$p_values)] = first_digit$p_values[rownames(first_digit$p_values), ]
  result_table['All Digits Test: Participants', ][rownames(ADT_BEN$p_values)] = ADT_BEN$p_values[rownames(ADT_BEN$p_values), ]
  result_table['Digit Pairs Test: Participants', ][colnames(digit_pair$p_values)] = digit_pair$p_values[colnames(digit_pair$p_values)]
  result_table['Rounding Test: Expenditure', ][colnames(rounding$p_values)] = rounding$p_values[colnames(rounding$p_values)]
  result_table['Repeats Test: Expenditure', ][colnames(repeats$p_values)] = repeats$p_values[colnames(repeats$p_values)]
  result_table['Sector Test: Expenditure (TRN_TRV_VEH)', ][colnames(sector$p_values)] = sector$p_values[colnames(sector$p_values)]
  result_table['High Low Test: Expenditure (2007)', ][rownames(high_low$p_values['2007'])] = high_low$p_values['2007'][rownames(high_low$p_values['2007']), ]
  result_table['Unpack Rounded Number Test: Participants (Round)', ][rownames(unpack$round)] = unpack$round[rownames(unpack$round), ]
  result_table['Padding Test: Expenditure', ] = NA #no simulation

  plots = list()
  plots[['All Digits Test: Expenditure']] = ADT_ALEXP$plots$AllBreakout
  plots[['First Digit Test: Expenditure']] = first_digit$plots$AllBreakout
  plots[['All Digits Test: Participants']] = ADT_BEN$plots$AllBreakout
  plots[['Digit Pairs Test: Participants']] = digit_pair$plot
  plots[['Rounding Test: Expenditure']] = rounding$plot
  plots[['Repeats Test: Expenditure']] = repeats$plot
  plots[['Sector Test: Expenditure (TRN_TRV_VEH)']] = sector$plot
  plots[['Sector Test: Expenditure (2007)']] = high_low$plot
  plots[['Unpack Rounded Number Test: Participants (Round)']] = unpack$plots$merged
  plots[['Padding Test: Expenditure']] = padding$AllBreakout$plot

  return(list(test_stats=result_table, plots=plots))
}

# result = replicate_EEL_10_tests(fp)



