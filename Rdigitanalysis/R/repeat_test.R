############################################################
#Functions for digit analysis R package
###repeat test functions in this file
#Wenjun Chang
#Summer 2020
############################################################

############################################################
#helper function
############################################################

#' Finds the percent of repeats in the given data based on given definition of a repeat (what columns need to match)
#'
#' @param dataThe The dataframe to compute percent repeat on
#' @inheritParams repeat_test
#'
#' @return The percentage of repeated numbers in input data
find_percent_repeats = function(data){
  #find repeats based on specified definition of a repeat
  unique_numbers = dim(unique(data))[1]
  total_numbers = dim(data)[1]
  num_repeats = total_numbers - unique_numbers
  percent_repeats = num_repeats / total_numbers

  return(percent_repeats)
}

################################################
################main function###################
################################################

####
#need to do failure factor
#need ADD plot parameter
####

#' Performs repeat test or sector effect test
#'
#' @param duplicate_matching_cols An array of names of data columns two rows need to match exactly in order to be defined as a repeat.
#' Default to 'all', meaning matching all columns in 'clean' slot of \code{digitdata}
#' @inheritParams all_digits_test
#'
#' @return
#' \itemize{
#'   \item A table of p-values for repeat test on each category possibly also on sector
#'   \item Plots for each category if \code{plot = TRUE}
#'   \item If NaN is in returned table, it means that there are no occurances of the data in that category --> 0/0 in percentage
#' }
#' @export
#'
#' @examples
#' repeat_test(digitdata)
#' repeat_test(digitdata, duplicate_matching_cols=c('col_name1, col_name2'))
#' repeat_test(digitdata, duplicate_matching_cols=c('col_name1, col_name2'), break_out='col_name')
repeat_test = function(digitdata, duplicate_matching_cols='all', break_out=NA){

  #check input
  input_check(digitdata=digitdata, break_out=break_out, duplicate_matching_cols=duplicate_matching_cols)

  #check if the sector columns are valid
  if (!(is.na(sector_column))){
    if (is.na(match(sector_column, colnames(digitdata@cleaned)))){
      stop('specified column to analyze sector effect is not a column in the data')
    }
    else{
      for (sector_name in names(sector_grouping)){
        if (NA %in% match(sector_grouping[[sector_name]], unique(digitdata@cleaned[[sector_column]]))){
          print(sector_name)
          stop('specified category is not a category in the column specified to analyze sector effect')
        }
      }
    }
  }
  #handles the duplicate_matching_cols = 'all' situation
  if (duplicate_matching_cols[1] == 'all'){
    duplicate_matching_cols = colnames(digitdata@cleaned)
  }
  #the columns we want to analyze
  data = digitdata@cleaned[duplicate_matching_cols]

  #percent repeats for all
  percent_repeats_all = find_percent_repeats(data)

  #df to store stats
  percent_repeats_table = data.frame(all=percent_repeats_all)

  #break out by category
  if (!(is.na(break_out))){
    #get indexes for each category
    indexes_of_categories = break_by_category(digitdata@cleaned, break_out) #this is a list since unequal number of entries for each category

    #break by category for all
    for (category_name in names(indexes_of_categories)){
      indexes_of_category = indexes_of_categories[[category_name]]
      data_of_category = data.frame(data[indexes_of_category, ])
      percent_repeats_in_category = find_percent_repeats(data_of_category)

      percent_repeats_table[category_name] = percent_repeats_in_category #a value
    }
  }
  #get the mean of all the values computed
  mean_percent_repeated = rowMeans(percent_repeats_table)
  percent_repeats_table['mean'] = mean_percent_repeated

  #create a rowname
  rownames(percent_repeats_table) = 'percent repeated numbers'
  #sort by decreasing rounded percentage
  percent_repeats_table = t(sort(percent_repeats_table, decreasing = TRUE))
  return(percent_repeats_table)
}

