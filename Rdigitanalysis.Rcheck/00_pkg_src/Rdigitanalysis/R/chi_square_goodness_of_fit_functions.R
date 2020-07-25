############################################################
#Functions for digit analysis R package
###chi square GOF function and degree of freedom functions for digit test functions in this file
#Wenjun Chang
#Summer 2020
############################################################


#find degree of freedom helper
get_df = function(table, standard=FALSE){
  #standard df = (r-1)(c-1)
  if (standard){
    df = (dim(table)[1]-1) * (dim(table)[2]-1)
    if (dim(table)[2] == 1){ #only 1 column: df = r-1
      df = dim(table)[1] - 1
    }
    return(df)
  }

  df = dim(table)[1] * dim(table)[2] - length(table)

  #if it include first digit place, one less df due to 0 has freq 0
  if (grepl('1', colnames(table)[1], fixed=TRUE)){
    df = df - 1
  }
  return(df)
}

#chi square test for goodness of fit
#freq denotes whether the expected table is in decimal
chi_square_gof = function(observed_table, expected_table, df, freq=TRUE){
  if (freq){
    #turn freq into numbers
    for (i in 1:length(expected_table)){
      expected_table[, i] = expected_table[, i] *sum(observed_table[, i])
    }
  }
  print(expected_table)

  #if first digit is used, turn digit 0 freq to 1 for both tables,
  #to avoid NaN in computing test stats
  # if (grepl('1', colnames(expected_table)[1], fixed=TRUE)){
  #   observed_table[1,1] = 1
  #   expected_table[1,1] = 1
  # }

  test_stats = sum((observed_table - expected_table)^2/expected_table, na.rm = TRUE)

  df = get_df(expected_table)

  p_value = pchisq(test_stats, df = df, lower.tail = FALSE) - dchisq(test_stats, df = df)

  return(p_value)
}

