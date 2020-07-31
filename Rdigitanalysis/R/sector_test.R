############################################################
#Functions for digit analysis R package
###repeat test functions in this file
#Wenjun Chang
#Summer 2020
############################################################

#' Performs sector test to analyze uneven distribution of percent repeats across sectors (supposed to be uniform).
#' A wrapper function for \code{repeat_test}.
#'
#' @param category_column The column for splitting the data into sectors for separate analysis. The second division (usually variables) shown in plots.
#' @param category_grouping A list of arrays
#' \itemize{
#'   \item Each the names of the elements in the list is the category name
#'   \item Each array contains the values belonging to that category
#' }
#' @param failure_factor NEED TO EDIT LATER
#' @inheritParams repeat_test
#'
#' @return
#' \itemize{
#'   \item A table of p-values for repeat test by sector on each category
#'   \item Plots for each break out element with variables as categories specified by \code{category_grouping} if \code{plot = TRUE}
#'   \item If NaN is in returned table, it means that there are no occurances of the data of the sector in that category --> 0/0 in percentage
#' }
#' @export
#'
#' @examples
#' sector_test(digitdata, category_column='sector_name', category_grouping=list('sector 1'=c('a'), 'sector 2'=c('b', 'c')))
#' sector_test(digitdata, category_column='sector_name', category_grouping=list('sector 1'=c('a, b'), 'sector 2'=c('c', 'd')),
#' duplicate_matching_cols=c('col_name1, col_name2'), break_out='col_name', failure_factor=3)
sector_test = function(digitdata, category_column, category_grouping, duplicate_matching_cols='all', break_out=NA, failure_factor=3){

  #check input
  input_check(digitdata=digitdata, break_out=break_out, duplicate_matching_cols=duplicate_matching_cols, category_column=category_column, category_grouping=category_grouping)

  #check if the sector columns are valid
  if (!(is.na(category_column))){
    if (is.na(match(category_column, colnames(digitdata@cleaned)))){
      stop('specified column to analyze sector effect is not a column in the data')
    }
    else{
      for (sector_name in names(category_grouping)){
        if (NA %in% match(category_grouping[[sector_name]], unique(digitdata@cleaned[[category_column]]))){
          print(sector_name)
          stop('specified category is not a category in the column specified to analyze sector effect')
        }
      }
    }
  }
  #initialize table to be returned
  category_names = c()
  if (!(is.na(break_out))){
    category_names = names(break_by_category(digitdata@cleaned, break_out))
  }
  sector_repeats_table = data.frame(matrix(nrow = length(category_names)+2, ncol = 0)) # +2 (all and mean)
  rownames(sector_repeats_table) = c('all', 'mean', category_names) #ensure each row is fixed for a category when append

  #get indexes for each category in the specified sector column
  sector_column_indexes = break_by_category(digitdata@cleaned, category_column) #this is a list since unequal number of entries for each category

  for (sector_name in names(category_grouping)){
    #get the index of sector by accessing the names of the categories in the data column that belong to this sector
    indexes_of_sector = sector_column_indexes[category_grouping[[sector_name]]]
    indexes_of_sector = unlist(indexes_of_sector) #turn into an array

    #create new digitdata object for each sector
    digitdata_of_sector = digitdata
    digitdata_of_sector@raw = data.frame() # save memory
    digitdata_of_sector@cleaned = data.frame(digitdata_of_sector@cleaned[indexes_of_sector, ])
    digitdata_of_sector@numbers = data.frame(digitdata_of_sector@numbers[indexes_of_sector, ])
    digitdata_of_sector@left_aligned = data.frame(digitdata_of_sector@left_aligned[indexes_of_sector, ])
    digitdata_of_sector@right_aligned = data.frame(digitdata_of_sector@right_aligned[indexes_of_sector, ])

    #repeats test
    repeats_table = repeat_test(digitdata_of_sector, duplicate_matching_cols=duplicate_matching_cols, break_out=break_out)

    #update table
    sector_repeats_table[[sector_name]][rownames(repeats_table)] = repeats_table #match the rownames by using rownames
  }
  return(t(sector_repeats_table)) #transpose it to look better
}

