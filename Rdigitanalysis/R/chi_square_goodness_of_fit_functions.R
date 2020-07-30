############################################################
#Functions for digit analysis R package
###chi square GOF function and degree of freedom functions for digit test functions in this file
#Wenjun Chang
#Summer 2020
############################################################


#' Compute the degrees of freedom for the input chi square test table
#'
#' @param table Either the expected or the observation table for the chi square test
#' @param standard TRUE or FALSE: Default to FALSE.
#' \itemize{
#'   \item TRUE: Compute degrees of freedom using formula df = (r-1)(c-1). If table has only one column, df = r - 1.
#'   \item FALSE: Compute degrees of freedom using df = r x (c-1). If first digit place is present, df = r x (c-1) - 1.
#' }
#' @return Degrees of freedom
get_df = function(table, standard=FALSE){
  df = NA
  #standard df = (r-1)(c-1)
  if (standard){
    df = (dim(table)[1]-1) * (dim(table)[2]-1)
    if (dim(table)[2] == 1){ #only 1 column: df = r-1
      df = dim(table)[1] - 1
    }
  }
  else{
    #df = r x (c-1)
    df = dim(table)[1] * dim(table)[2] - length(table)
  }
  #if it include first digit place, one less df due to 0 has freq 0
  if (grepl('1', colnames(table)[1], fixed=TRUE)){
    df = df - 1
  }
  return(df)
}


#' Perform chi square test for goodness of fit test
#'
#' @param observed_table Observation table for chi square test
#' @param expected_table Expected table for chi square test
#' @param df Degrees of freedom
#' @param freq TRUE or FALSE: Whether the input expected table is in decimal.
#'
#' @return p-value for chi square goodness of fit test
#' @export
chi_square_gof = function(observed_table, expected_table, df, freq=TRUE){
  if (freq){
    #turn freq into numbers
    for (i in 1:length(expected_table)){
      expected_table[, i] = expected_table[, i] *sum(observed_table[, i])
    }
  }
  test_stats = sum((observed_table - expected_table)^2/expected_table, na.rm = TRUE)
  # print('df')
  # print(df)
  # print('test_stats')
  # print(test_stats)

  p_value = pchisq(test_stats, df = df, lower.tail = FALSE)

  return(p_value)
}


