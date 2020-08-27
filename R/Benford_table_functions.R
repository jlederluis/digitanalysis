############################################################
# DigitAnalysis R Package
# github.com/jlederluis/digitanalysis
# Jetson Leder-Luis and Jean Ensminger
# Research assistant: Wenjun Chang
# Benford table create, save, and load function according to Benford's Law in this file
############################################################

#' Generates contingency table for Benford distribution (a.k.a expected digit frequency under Benford's Law).
#' Columns are digit place in increasing order, and rows are the digits (0 to 9) in increasing order
#'
#' @param N Number of digits to generate
#' \itemize{
#'   \item N <= 6 takes instant time
#'   \item N = 7 takes 5 mins
#'   \item N = 8 takes 30 mins
#' }
#' @param out_fp Filepath to save the table to. Default to NA.
#' @param save TRUE or FALSE: saving the table or not. Default to FALSE.
#'
#' @return Benford contingency table generated.
#' @export
#'
#' @examples
#' Benford_table(N)
#' Benford_table(N, out_fp='~/filepath', save=TRUE)
Benford_table = function(N, out_fp=NA, save=FALSE){
  if (save){
    if (is.na(out_fp)){
      stop('If save is TRUE, must specify out_fp!')
    }
  }
  if (!(is.na(out_fp))){
    if (!(save)){
      stop('If specified out_fp, save must be TRUE!')
    }
  }
  contingency_table = data.frame(Digits=0:9)
  for (n in 1:N){
    if (n == 1){
      #first digit we hard code
      current_freqs = rep(0, 10)
      for (digit in 1:9){
        current_freqs[digit+1] = log10(1+1/digit)
      }
      #update table
      contingency_table[[paste('Digit Place', as.character(n))]] = current_freqs
    }
    else {
      #digits place greater than the first
      args = list(1:9) #first digit has no 0
      args = append(args, rep(list(0:9), n-2))
      #create all possible combinations of the prefix digits
      n_digits_combinations = expand.grid(args)
      n_digits_combinations = apply(n_digits_combinations, 1, paste, collapse='')

      current_freqs = rep(NA, 10)
      for (digit in 0:9){
        freq = 0
        for (prefix in n_digits_combinations){
          denom = as.integer(paste(prefix,as.character(digit),sep=''))
          freq = freq + log10(1+1/as.integer(denom))
        }
        current_freqs[digit+1] = freq
      }
      #update table
      contingency_table[[paste('Digit Place', as.character(n))]] = current_freqs
    }
  }
  #drop the weird "X" column in df if loading a dataframe in R
  contingency_table = contingency_table[!(colnames(contingency_table) %in% c("X"))]

  #save file if specified
  if (save) {
    write.csv(contingency_table, out_fp)
  }
  return(contingency_table)
}

#' Loads Benford contingency table
#'
#' @param table_fp Filepath where the developers storing the table
#'
#' @return The loaded Benford table
load_Benford_table = function(table_fp){
  contingency_table = read.csv(table_fp)
  #get rid of '.' replacing ' ' problem when loading csv to df
  colnames(contingency_table) = gsub("."," ",colnames(contingency_table), fixed=TRUE)
  #name the rows from 0 to 9
  rownames(contingency_table) = 0:9
  return(contingency_table)
}

