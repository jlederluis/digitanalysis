############################################################
#Functions for digit analysis R package
###repeat test functions in this file
#Wenjun Chang
#Summer 2020
############################################################

####failure factor

#' Performs sector test to analyze uneven distribution of percent repeats across sectors (supposed to be uniform).
#' A wrapper function for \code{repeat_test}.
#'
#' @param category The column for splitting the data into sectors for separate analysis. The second division (usually variables) shown in plots.
#' @param category_grouping A list of arrays, or defaulted to NA.
#' \itemize{
#'   \item Each the names of the elements in the list is the category name
#'   \item Each array contains the values belonging to that category
#'   \item If it is remain as NA as default, while \{category} is not NA, then \code{category_grouping} will default to every individual item in
#'   \code{category} will be in a separate group.
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
#' sector_test(digitdata, category='sector_name', category_grouping=list('sector 1'=c('a'), 'sector 2'=c('b', 'c')))
#' sector_test(digitdata, category='sector_name', category_grouping=list('sector 1'=c('a, b'), 'sector 2'=c('c', 'd')),
#' duplicate_matching_cols=c('col_name1, col_name2'), break_out='col_name', failure_factor=3)
sector_test = function(digitdata, category, category_grouping=NA, duplicate_matching_cols='all', break_out=NA, failure_factor=3, plot=TRUE){

  #check input
  input_check(digitdata=digitdata, break_out=break_out, duplicate_matching_cols=duplicate_matching_cols, category=category, category_grouping=category_grouping, plot=plot)

  #check if the sector columns are valid
  if (is.na(match(category, colnames(digitdata@cleaned)))){
    stop('specified column to break on second division is not a column in the data!')
  }
  else{
    if (!(is.na(category_grouping))){
      for (category_name in names(category_grouping)){
        if (NA %in% match(category_grouping[[category_name]], unique(digitdata@cleaned[[category]]))){
          print(category_name)
          stop('specified category is not a category in the column break on second division!')
        }
      }
    }
  }
  #handles if grouping is NA, while group is not
  category_grouping = get_grouping(grouping=category_grouping, column=category, digitdata=digitdata)

  #initialize table to be returned
  category_names = c()
  if (!(is.na(break_out))){
    category_names = names(break_by_category(digitdata@cleaned, break_out))
  }
  sector_repeats_table = data.frame(matrix(nrow = length(category_names)+1, ncol = 0)) # +1 (all)
  rownames(sector_repeats_table) = c('All', category_names) #ensure each row is fixed for a category when append

  #get indexes for each category in the specified sector column
  sector_column_indexes = break_by_category(digitdata@cleaned, category) #this is a list since unequal number of entries for each category

  for (sector_name in names(category_grouping)){
    print(sector_name)

    #get the index of sector by accessing the names of the categories in the data column that belong to this sector
    indexes_of_sector = sector_column_indexes[category_grouping[[sector_name]]]
    indexes_of_sector = unlist(indexes_of_sector) #turn into an array

    #create new digitdata object for each sector
    digitdata_of_sector = make_sub_digitdata(digitdata=digitdata, indexes=indexes_of_sector)

    #repeats test
    repeats_table = repeat_test(digitdata_of_sector, duplicate_matching_cols=duplicate_matching_cols, break_out=break_out)
    #update table
    print(repeats_table)

    sector_repeats_table[sector_name] = NA
    sector_repeats_table[sector_name][rownames(repeats_table), ] = repeats_table #match the rownames by using rownames
    # return(list(a=sector_repeats_table, b=repeats_table))

    print(sector_repeats_table)
  }

  #plot
  if (plot){
    print(hist_2D_variables(data.frame(sector_repeats_table), data_style='col', xlab='districts', ylab='percent repeats', title='Sector Effect Test'))
  }
  return(sector_repeats_table)
}

