############################################################
#Functions for digit analysis R package
###chi square GOF function and degree of freedom functions for digit test functions in this file
#Wenjun Chang
#Summer 2020
############################################################


#' Drops disqualified columns in both expected and observed table if in expected table at least one cell
#' in that column has expected value < 5, conforming to the principles of chi square test.
#'
#' @inheritParams chi_square_gof
#'
#' @return A list with mofidied \code{observed_table} and \code{expected_table}
drop_disqualified_columns = function(observed_table, expected_table, freq=TRUE){
  if (freq){
    #turn freq into numbers
    for (i in 1:length(expected_table)){
      expected_table[, i] = expected_table[, i] *sum(observed_table[, i])
    }
  }
  #suppress columns in expected table if at least one cell in that column has expected value < 5
  disqualified_columns = c()
  for (i in 1:length(expected_table)){
    if (TRUE %in% (expected_table[i] < 5)){
      disqualified_columns = c(disqualified_columns, i)
    }
  }
  #drop disqualified columns in both expected and observed table
  if (length(disqualified_columns) > 0){
    expected_table = expected_table[-disqualified_columns]
    observed_table = observed_table[-disqualified_columns]
  }
  #stop if all columns have entries < 5, cannot perform chi square test
  if (ncol(expected_table) == 0){
    stop('cannot proceed to perform chi square test because all digit places have at least one expecetd value < 5,
         which violates the principle of chi square test!')
  }
  return(list(observed_table=observed_table, expected_table=expected_table))
}


#' Compute the degrees of freedom for the input chi square test table
#'
#' @param table Either the expected or the observation table for the chi square test
#' @inheritParams chi_square_gof
#'
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


#' Performs chi square test for goodness of fit test. Plots \code{expected_table} with 2D and/or 3D histograms accordingly if specified.
#'
#' @param observed_table Observation table for chi square test
#' @param expected_table Expected table for chi square test
#' @param freq TRUE or FALSE: Whether the input expected table is in decimal.
#' @param standard TRUE or FALSE: Default to FALSE.
#' \itemize{
#'   \item TRUE: Compute degrees of freedom using formula df = (r-1)(c-1). If table has only one column, df = r - 1.
#'   \item FALSE: Compute degrees of freedom using df = r x (c-1). If first digit place is present, df = r x (c-1) - 1.
#' }
#' @inheritParams all_digits_test
#'
#' @return p-value for chi square goodness of fit test
#' @export
chi_square_gof = function(observed_table, expected_table, freq=TRUE, suppress_low_N=TRUE, standard=FALSE, plot=TRUE){

  #for subsets on break_out, there might be columns that are all zeros in sub-observation table
  #that needs to be removed
  zero_columns = which(colSums(observed_table) == 0)
  if (length(zero_columns) > 0){
    #remove zero columns and recompute df if necessary when getting the p values
    observed_table = observed_table[-zero_columns]
    expected_table = expected_table[-zero_columns]
  }

  #suppress columns in expected table if at least one cell in that column has expected value < 5
  if (suppress_low_N){
    tables_lst = drop_disqualified_columns(observed_table, expected_table, freq=freq)
    observed_table = tables_lst$observed_table
    expected_table = tables_lst$expected_table
  }
  else {
    #turn expected table into counts instead of frequency
    if (freq){
      #turn freq into numbers
      for (i in 1:length(expected_table)){
        expected_table[, i] = expected_table[, i] *sum(observed_table[, i])
      }
    }
  }
  #compute df
  df = get_df(observed_table, standard=standard)

  #chi square test
  test_stats = sum((observed_table - expected_table)^2/expected_table, na.rm = TRUE)
  p_value = pchisq(test_stats, df = df, lower.tail = FALSE)
  return(list(p_value=p_value, expected_table=expected_table, observed_table=observed_table))
}

